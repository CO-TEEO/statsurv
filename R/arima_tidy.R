#' @title Fit ARIMA models via formula
#'
#' @description `arima_tidy` is a wrapper around \code{\link[arima]{stats}} that allows you to specify the
#' response variable and any external regressors for an ARIMA model using a data.frame and formula,
#' instead of having to manually generate `xreg`. This makes the workflow for fitting ARIMA models
#' much closer to the workflow for fitting `lm` or `glm` models.
#'
#'
#' @param f A \code{\link{stats::formula}} describing the response variable and any external
#'   regressors to be used in the arima model. All variables included in `f` must be contained in
#'   `data`. The response variable is passed to \code{\link[arima]{stats}} as `x` and all external
#'   regressors are passed as `xreg`.
#' @param data A data frame or data frame extension (e.g., a tibble) containing all of the variables
#'   in the model
#' @param seasonal A specification of the seasonal part of the ARIMA model, plus the period. This
#'   must be specified by a list with components order and period.
#' @inheritParams stats::arima
#'
#'
#' @return A model fit object of class "arima_tidy". This is an extension of the Arima model fit
#'   produced by \code{\link[stats]{arima}}, so functions such as \code{\link[stats]{predict}} will
#'   work identically. The output of `arima_tidy` contains some additional components that make
#'   working with them easier:
#'   \describe{
#'   \item{formula}{The formula used to fit the model}
#'   \item{data}{The data used to fit the model}
#'   \item{xreg}{The generated values of `xreg` used as external regressors}
#'   \item{fitted}{The fitted values produced by the model. The output of \code{\link[stats]{arima}}
#'   contains the residuals, but not the fitted values.}
#'   }
#' @export
#' @md
#' @seealso \code{\link[stats]{arima}}, \code{\link{predict.arima_tidy}}, \code{\link{augment.arima_tidy}}
#'
#' @examples
#' data("LakeHuron")
#' LakeHuron_df <- data.frame(lake_level = as.numeric(LakeHuron),
#'                            date = seq(start(LakeHuron)[[1]],
#'                                       end(LakeHuron)[[1]],
#'                                       by = 1))
#' LakeHuron_df$election_year = LakeHuron_df$date %% 4 == 0
#'
#' # Basically anything that works in a formula can be used in arima_tidy
#' arima_tidy(lake_level ~ I(date - 1920), data = LakeHuron_df, order = c(2, 0, 0))
#'
#' arima_tidy(sqrt(y) ~ date + is_election_year, data = LakeHuron_df, order = c(2, 0, 0))
#'
#' # We can reference variables outside of the `data` argument
#'
#' shift <- 2000
#' arima_tidy(sqrt(y) ~ log(date + shift),  data = LakeHuron_df, order = c(2, 0, 0))
#'
#' # We can handle data with NA's and seasonal data:
#' data("presidents_df")
#'
#' arima_tidy(y ~ 1, data = presidents_df, order = c(1, 0, 0))
#'
#' # We lose the frequency information when the data is
#' # stored in a data.frame, so we have to specify
#' # the period manually for seasonal approaches:
#' arima_tidy(y ~ 1, data = presidents_df, order=c(2,0,1),
#'            seasonal= list(order = c(1,0,0), period = 4),
#'            fixed=c(NA, NA, 0.5, -0.1, 50), transform.pars=FALSE)
arima_tidy <- function(f, data,
                       order = c(0L, 0L, 0L),
                       seasonal = list(order = c(0L, 0L, 0L), period = NA),
                       include.mean = TRUE,
                       transform.pars = TRUE,
                       fixed = NULL,
                       init = NULL,
                       method = c("CSS-ML", "ML", "CSS"),
                       n.cond,
                       SSint = c("Gardner1980", "Rossignol2011"),
                       optim.method = "BFGS",
                       optim.control = list(), kappa = 1e6) {
  # I don't love this approach, because it will break if the arguments to arima change. I feel like
  # there should be a way to set the arguments programatically such as through formals(arima_tidy)
  # <- formals(arima), but I'm not sure how that would work with roxygen

  # Arg checks
  stopifnot(rlang::is_formula(f),
            is.data.frame(data))

  # Code
  xreg <- gen_xreg(f, data)
  resp <- gen_resp(f, data)

  if (ncol(xreg) == 0) {
    fit <- arima(resp, xreg = NULL, order, seasonal, include.mean, transform.pars, fixed,
                 init, method, n.cond, SSint, optim.method, optim.control, kappa)
  } else {
    fit <- arima(resp, xreg = xreg, order, seasonal, include.mean, transform.pars, fixed,
                 init, method, n.cond, SSint, optim.method, optim.control, kappa)
  }
  fit$formula <- f
  fit$xreg <- xreg
  fit$data <- data
  fit$fitted <- resp - fit$residuals
  class(fit) <- c("arima_tidy", class(fit))
  fit
}


#' Forecast from arima_tidy fits
#'
#' Forecast from models fit by \code{\link{arima_tidy}}. Calls \code{\link[stats]{predict.Arima}}
#' under the hood.
#'
#' Even though `arima_tidy` models are fit like `lm` or `glm` models, it's important to remember
#' that they behave differently, because the order of the data points is crucial for all ARIMA
#' models. The rows in `only_newdata` are assumed to be ordered from earliest to latest, and are
#' assumed to follow immeadiately after the data used to fit the model. To extract fitted values and
#' generate predictions in a single step, use \code{\link{augment.arima_tidy}}.
#'
#' @param object The result of an \code{arima_tidy} fit
#'
#' @param only_newdata New data to be used for prediction. Will be transformed into `newxreg` using
#'   the formula stored in `object`.
#' @param n.ahead The number of steps ahead for which prediction is required. Optional if
#'   `only_newdata` is specified, otherwise required.
#' @inheritParams stats::predict.Arima
#'
#' @return A time series of predictions, or if `se.fit = TRUE`, a list with timeseries `pred` and
#'   `se`.
#' @seealso \code{\link[stats]{arima}}, \code{\link[stats]{predict.Arima}}, \code{\link{augment.arima_tidy}}
#' @export
#' @md
#' @examples
#' data(presidents_df)
#' # Fit on the first 110 rows, predict the last 10
#' fit <- arima_tidy(approval_rating ~ 1,
#'                   data = presidents_df[1:110, ],
#'                   order = c(1, 0, 0))
#' predict(fit, n.ahead = 10)
#'
#' fit <- arima_tidy(approval_rating ~ election_year,
#'                   data = presidents_df[1:110, ],
#'                   order = c(1, 0, 0))
#' predict(fit, only_newdata = presidents_df[111:120, ], n.ahead = 10)
#' predict(fit, only_newdata = presidents_df[111:120, ])
predict.arima_tidy <- function(object, only_newdata = NULL, n.ahead, se.fit = TRUE, ...) {
  stopifnot(inherits(object, "arima_tidy"),
            inherits(object, "Arima"),
            rlang::is_scalar_logical(se.fit),
            is.null(only_newdata) || is.data.frame(only_newdata))
  if (!is.null(only_newdata) & !missing(n.ahead)) {
    stopifnot(rlang::is_scalar_integerish(n.ahead))
  }
  args <- list(...)
  if ("newxreg" %in% names(args)) {
    stop("predict cannot be called on `arima_tidy` objects using `newxreg`. ",
         "Specify new data for predictions using the `only_newdata` argument")
  }


  # Need this because the predict.Arima method tries to pull xreg out of the parent environment.
  # Why you ask? I have no idea.
  xreg <- object$xreg

  if (!is.null(only_newdata)) {
    comb_data <- rbind(object$data, only_newdata)
    newxreg <- gen_xreg(object$formula, comb_data)
    old_rows <- seq(1, nrow(object$data), by = 1)
    only_newxreg <- newxreg[-old_rows, , drop = FALSE]
    if (missing(n.ahead)) {
      n.ahead <- nrow(only_newxreg)
    }

    # We do this manually, instead of calling NextMethod because
    # calling NextMethod leads to a different parent environment than
    # when we do it manually.
    class(object) <- class(object)[-1]
    predict(object, newxreg = only_newxreg, n.ahead = n.ahead, ...)
  } else {
    NextMethod("predict", object, n.ahead = n.ahead, ...)
  }
}

#' Augment data with information from an arima_tidy object
#'
#' A method for the \code{\link[generics]{augment}} generic for \code{\link{arima_tidy}} objects.
#' Takes a model object and a dataset, and adds information about fitted values in the `.fitted`
#' column and model residuals in the `.resid` column.
#'
#' @inheritParams predict.arima_tidy
#' @param newdata The data used to originally fit the model as well as any additional data to be
#'   used to generate predictions. The rows are assumed to be ordered from earliest to latest. If
#'   the originally model was fit on `n` data points, the first `n` rows of `newdata` are assumed to
#'   be the data used to fit the model, and any additional rows are used to generate predictions
#'   using \code{\link{predict.arima_tidy}}.
#' @param ... Additional arguments. Not used.
#'
#' @return `newdata` with additional columns `.fitted` and `.resid` containing the predicted values
#'   and the residuals.
#' @export
#' @md
#' @examples
#' data(presidents_df)
#' # Fit on the first 110 rows, predict the last 10
#' fit <- arima_tidy(approval_rating ~ 1,
#'                   data = presidents_df[1:110, ],
#'                   order = c(1, 0, 0))
#' # Augment takes the whole dataset (both training and prediction data)
#' augment(fit, presidents_df)
#'
#' fit <- arima_tidy(approval_rating ~ election_year,
#'                   data = presidents_df[1:110, ],
#'                   order = c(1, 0, 0))
#' augment(fit, presidents_df)
augment.arima_tidy <- function(fit, newdata, ...) {
  # TODO(): Document this
  stopifnot(inherits(fit, "arima_tidy"),
            inherits(fit, "Arima"),
            is.data.frame(newdata))

  n_ahead <- nrow(newdata) - length(fit$fitted)
  if (n_ahead < 0) {
    stop("newdata must contain at least as many rows as was used to originally fit the model")
  }
  n_old <- length(fit$fitted)
  old_predictions <- as.numeric(fit$fitted)

  if (n_ahead > 0) {
    only_newdata <- newdata[n_old + seq_len(n_ahead), , drop = FALSE]
    new_predictions <- as.numeric(stats::predict(fit, only_newdata = only_newdata)$pred)
    predictions <- c(old_predictions, new_predictions)
  } else {
    predictions <- old_predictions
  }
  newdata$.fitted <- predictions
  resp <- gen_resp(fit$formula, newdata)
  newdata$.resid <- resp - newdata$.fitted
  return(newdata)
}


gen_xreg <- function(f, data) {
  frame <- model.frame(f, data = data, na.action = NULL)
  matrix <- model.matrix(f, data = frame)
  m <- colnames(matrix) != "(Intercept)"
  matrix[, m, drop = FALSE]
}

gen_resp <- function(f, data) {
  model.response(model.frame(f, data = data, na.action = NULL))
}
