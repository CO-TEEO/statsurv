#' @title Extract fitted values from a model object
#'
#' @description A consistent method for extracting fitted values from a model fit object. Generally
#'   a wrapper around \code{\link[generics]{augment}} from the broom package, but one that always
#'   returns the values on the scale of the response variable. `extract_yhat` also covers `arima`
#'   and `INLA` models that `augment` does not. `extract_yhat` is a generic, meaning that the method
#'   invoked depends on what type of model is passed as the first argument.
#'
#'
#' @param fit A model fit object, such as that generated by \code{\link[stats]{lm}} or
#'   \code{\link[stats]{glm}}.
#' @param newdata A data frame containing the data that should be used to predict the fitted values.
#'   Must have all of the covariates used by the model fit object to calculate fitted values.
#' @param se_fit If TRUE, also calculate the standard errors of the fitted values. Not available for
#'   all methods.
#' @param ... Additional arguments passed into other methods
#'
#' @details Where possible, `extract_yhat` passes its arguments to \code{\link[generics]{augment}},
#'   calling `augment(x = fit, newdata = newdata, type.predict = "response")`. In some cases, such
#'   as for \code{\link[lme4]{glmer}} models, `extract_yhat` takes additional steps to better handle
#'   `NA` values in the original dataset used to fit the model. `extract_yhat` can also extract
#'   fitted values from arima models or models from the `INLA` package that do not have `augment`
#'   methods. See below for details on each individual method.
#'
#' @section Method Details:
#' \itemize{
#'    \item{\strong{\code{lm, glm:}} Identical to `augment(x = fit, newdata = newdata, type.predict
#'    = "response")`}
#'    \item{\strong{\code{lmer, glmer:}} Starts by calling `augment`, but removes the columns ".mu",
#'    ".offset", ".sqrtXwt", ".sqrtrwt", ".weights", ".wtres", ".gam", and ".eta" from the result.
#'    This is because these columns aren't necessary for statistical surveillance, and because they
#'    have errors when the original data frame contains NA's. In addition, `extract_yhat` gives an
#'    error if offset is included as an option in glmer. As a workaround, offsets can be specified
#'    as a term in the model formula, via `offset(VAR)`}
#'    \item{\strong{\code{INLA:}} Uses the linear predictor calculated by INLA and the link function
#'    to calculated fitted values. The model must be fit with the option
#'    `control.predictor=list(compute = TRUE)` in order for `extract_yhat` to work. Can be called
#'    with the option `se_fit = TRUE`, which gives standard errors for the fitted values, but note
#'    that this can be extremely slow.}
#'    \item{\strong{Arima models:} See \code{\link{extract_yhat.Arima}} }
#'}
#' @return The input data frame `newdata` with the added column `.fitted` containing the predicted
#'   values. If `se_fit = TRUE`, then the data frame also contains the column `.se.fit` with the
#'   errors of the predicted values. Some models may also return additional columns, such as
#'   `.resid`. All columns added by `extract_yhat` or `augment` start with a dot.
#'
#' @export
#' @seealso \code{\link{extract_yhat.Arima}} \code{\link[broom]{augment.lm}}
#' @md
#' @examples
#' library("scanstatistics")
#' library("broom")
#' library("broom.mixed")
#' data(NM_popcas)
#'
#' # Fit a model to all the data, and then extract predictions:
#' fit <- glm(count ~ year,
#'            family = poisson(link = "log"),
#'            offset = log(population),
#'            data = NM_popcas[1:600, ])
#'
#' # Then use extract_yhat to get out predictions for our observed variable:
#' extract_yhat(fit, NM_popcas, se_fit = TRUE)
#'
#' # We can also fit glmer models
#' fit_glmer <- lme4::glmer(count ~ I(year - 1980) + (1 | county) + offset(log(population)),
#'                          family = "poisson",
#'                          data = NM_popcas[1:600, ])
#' extract_yhat(fit_glmer, NM_popcas, se_fit = TRUE)
#'
#' \dontrun{
#'   # We can also use INLA models if available:
#'   # All the data points to generate predictions for much be included in the
#'   # data arg of the INLA model
#'   fit_inla <- INLA::inla(count ~ year + f(county, model = "iid"),
#'                          family = "poisson",
#'                          control.family = list(link = "log"),
#'                          offset = log(population),
#'                          # This option is required to use extract_yhat
#'                          control.predictor = list(compute = TRUE),
#'                          data = NM_popcas)
#'
#'   extract_yhat(fit_inla, NM_popcas)
#' }
extract_yhat <- function(fit, newdata, ...) {
 UseMethod("extract_yhat", fit)
}

#' @rdname extract_yhat.Arima
#' @export
extract_yhat.arima_tidy <- function(fit, newdata, ...) {
  # Having another method dispatch was confusing roxygen.
  # So we're calling this directly.
  augment.arima_tidy(fit, newdata, ...)
}

#' @export
extract_yhat.default <- function(fit, newdata, ...) {
  generics::augment(fit, newdata = newdata, type.predict = "response", ...)
}


# lm, glm, and merMod should be handled by broom and broom.mixed
# So the key ones to handle are INLA and forecast_ARIMA, if I can.
# Turns out merMod doesn't handle NA's well.
# (Well, the .fitted column is ok, it's all the other columns that mess us up)

#' @rdname extract_yhat
#' @export
extract_yhat.merMod <- function(fit, newdata, ...) {
  if ("(offset)" %in% colnames(stats::model.frame(fit))) {
    stop("predict.merMod does not work if an offset is included as an option in glmer.",
         " This is a known bug in the lme4 package.",
         "As a workaround, offsets can be specified as a term in the model formula ",
         "via 'offset(VAR)'")
  }
  res <- suppressWarnings(generics::augment(fit, newdata = newdata, type.predict = "response", ...))
  possible_bad_cols <- c(".mu", ".offset", ".sqrtXwt",
                         ".sqrtrwt", ".weights", ".wtres", ".gam",
                         ".eta")
  bad_cols <- which(colnames(res) %in% possible_bad_cols)
  res[, -bad_cols]
}


#' @rdname extract_yhat
#' @export
extract_yhat.inla <- function(fit, newdata, se_fit = FALSE, ...) {
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("The 'INLA' package is required to use inla models in statsurv; ",
         "please install it before continuing")
  }
  # Depending on how INLA is set up, we can get lots of different values back
  # I'm always going to be using the linear predictor.
  if (is.null(fit$marginals.linear.predictor)) {
    stop("Unable to extract fitted values from this inla-object. ",
         "The inla-object must be computed with options ",
         "'control.predictor=list(compute = TRUE)' and ",
         "'control.compute=list(return.marginals.predictor=TRUE)' to extract fitted values.")
  }

  inla_link <- get_inla_link(fit)
  inv_link <- function(x) {
    inla_link(x, inverse = TRUE)
  }
  linear_predictor_marginals <- fit$marginals.linear.predictor

  if (se_fit) {
    transformed_marginals <- purrr::map(linear_predictor_marginals,
                                        INLA::inla.tmarginal,
                                        fun = inv_link)
    fitted_values <- purrr::map_dfr(transformed_marginals, INLA::inla.zmarginal, silent = TRUE)
    fitted_mean <- fitted_values$mean
    fitted_sd <- fitted_values$sd
    if (!is.null(fit$.args$E)) {
      # Fitted values does not include the exposure, so we need to account for that
      fitted_mean <- fitted_mean * fit$.args$E
      fitted_sd <- fitted_sd * fit$.args$E
    }
    newdata$.fitted <- fitted_mean
    newdata$.se.fit <- fitted_sd
  } else {
    fitted_mean <- purrr::map_dbl(linear_predictor_marginals, INLA::inla.emarginal, fun = inv_link)
    if (!is.null(fit$.args$E)) {
      # Fitted values does not include the exposure, so we need to account for that
      fitted_mean <- fitted_mean * fit$.args$E
    }
    newdata$.fitted <- fitted_mean
  }

  response_var <- all.vars(fit$.args$formula)[[1]]
  if (response_var %in% names(newdata)) {
    newdata$.resid <- newdata[[response_var]] - newdata$.fitted
  }
  return(newdata)
}


get_inla_link  <- function(fit) {
  family <- fit$.args$family
  link_name <- fit$.args$control.family[[1]]$link
  # Now we do some terrible things with INLA and evaluating character strings
  if (link_name == "default") {
    link_name <- INLA::inla.models()$likelihood[[family]]$link[[2]]
  }
  function_name <- paste0("INLA::",
                          "inla.link.",
                          link_name)
  inla_link <- eval(parse(text = function_name))
  return(inla_link)
}
