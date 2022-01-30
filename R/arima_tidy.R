#' Fit ARIMA models via formula
#'
#' `arima_tidy` is a wrapper around \code{\link[arima]{stats} that allows you to specify the
#' response variable and any external regressors for an ARIMA model using a data.frame and formula,
#' instead of having to manually generate `xreg`. This makes the workflow for fitting ARIMA models
#' much closers to the workflow for fitting `lm` or `glm` models.
#'
#'
#' @param f A \code{\link{stats::formula}} describing the response variable and any external
#'   regressors to be used in the arima model. All variables included in `f` must be contained in
#'   `data`. The response variable is passed to \code{\link[arima]{stats} as `x` and all external
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
#'
#' @examples
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
  #
  xreg <- gen_xreg(f, data)
  resp <- gen_resp(f, data)
  fit <- arima(resp, xreg = xreg, order, seasonal, include.mean, transform.pars, fixed,
               init, method, n.cond, SSint, optim.method, optim.control, kappa)
  fit$formula <- f
  fit$xreg <- xreg
  fit$data <- data
  fit$fitted <- resp - fit$residuals
  class(fit) <- c("arima_tidy", class(fit))
  fit
}


#' @export
predict.arima_tidy <- function(object, only_newdata = NULL, newxreg = NULL, ...) {
  # TODO(): Document this

  # Need this because the predict.Arima method tries to pull xreg out of the parent environment.
  # Why you ask? I have no idea.
  xreg <- object$xreg

  if (!is.null(newxreg)) {
    NextMethod("predict", object)
  } else {
    # Then allow this to be able to provide new data, instead of newxreg.
    comb_data <- rbind(object$data, only_newdata)
    newxreg <- gen_xreg(object$formula, comb_data)
    old_rows <- seq(1, nrow(object$data), by = 1)
    newnewxreg <- newxreg[-old_rows, , drop = FALSE]
    NextMethod("predict", object, newxreg = newnewxreg)
  }
}

#' @export
augment.arima_tidy <- function(fit, newdata, ...) {
  # TODO(): Document this

  n_ahead <- nrow(newdata) - length(fit$fitted)
  n_old <- length(fit$fitted)
  newxreg <- gen_xreg(fit$formula, newdata)
  xreg <- fit$xreg
  # We have to assume that the rows of newdata are ordered, otherwise this doesn't work.

  newnew_xreg <- newxreg[n_old + seq_len(n_ahead), , drop = FALSE]
  if (ncol(newnew_xreg) == 0) {
    newnew_xreg <- NULL
  }
  new_predictions <- as.numeric(stats::predict(fit,
                                               n.ahead = n_ahead,
                                               newxreg = newnew_xreg)$pred)
  old_predictions <- as.numeric(fit$fitted)
  predictions <- c(old_predictions, new_predictions) #I don't know if we can assume this
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
