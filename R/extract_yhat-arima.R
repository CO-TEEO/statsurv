#' Extract fitted values from an ARIMA model
#'
#' @description  Methods for extracting fitted values from ARIMA models, including those fit using
#' \code{\link[stats:arima]{stats::arima}}, \code{\link[forecast:Arima]{forecast::Arima}}, or
#' \code{\link{arima2}}.
#'
#' Compared to other types of models, extrating fitted values from Arima models is rather fiddly. In
#' particular, the *order* of rows in `newdata` and `newxreg` is crucially important. If the ARIMA
#' model was originally fit on `n` points, `extract_yhat` assumes that the first `n` rows of
#' `newdata` and `newxreg` correspond exactly to the data used to fit the model. The number of
#' additional rows in `newdata` is taken as the number of steps ahead that predictions should be
#' generated.
#'
#' @details Behind the scenes, `extract_yhat` is performing two different tasks: extracting already
#'   fitted values from the ARIMA model, and forecasting future values of the response variable. For
#'   most other models, these two tasks are the same, since once the model is fit, the output of one
#'   data point is not dependent on any other data points. The fact that the predictions of an ARIMA
#'   model depend on the order of the data points drives a lot of the oddities of `extract_yhat` for
#'   ARIMA models.
#'
#'   Extracting already fitted values is simple for models fit with the `forecast::Arima` or
#'   `arima_tidy` functions, because the fitted values are stored in the model fit object. Models
#'   fit with `stats::arima` do *not* store this information, and so this information must be
#'   supplied to `extract_yhat` by through the parameters `newdata` and `resp_var`.
#'
#'   `extract_yhat` can only be used to predict fitted values conditional on the data used to fit
#'   the model. Any covariates necessary to generate these forecasts must be provided as the last
#'   rows in `newxreg`. Although `newdata` is a required parameter to the functions, it is not used
#'   in generating the predictions, and only used to report the final result. This is done so that
#'   the output from `extract_yhat` is consistent with the output from `augment` and the broader
#'   Tidyverse, which uses data frames as the key interface for passing data between functions.
#'
#'   For a version of arima that is based entirely on data frames, see \code{\link{arima_tidy}}.
#'
#'
#'
#'
#' @param fit An ARIMA model fit object.
#' @param newdata A data frame containing the data that should be used to predict the fitted values.
#'   Must have all of the covariates used by the model fit object to calculate fitted values. The first `n` rows should be exactly the data used to fit the model, and any additional rows should correspond to the number of steps ahead that predictions should be generated.
#' @param resp_var The column in `newdata` containing the time series data the model was fitted on.
#'   Only used for `stats::arima` models. The column should be specified as bare text (no quotes).
#' @param newxreg A matrix of covariate information containing both the values of `xreg` used to
#'   originally fit the model, and any new values of xreg to be used for prediction. IMPORTANT:
#'   Unlike \code{\link[stats]{predict.Arima}}, `newxreg` must contain both the old and new values
#'   of xreg.
#' @param ...
#'

#' @return The input data frame `newdata` with the added column `.fitted` containing the predicted
#'   values. Some models may also return additional columns, such as `.resid`. All columns added by
#'   `extract_yhat` or `augment` start with a dot.
#' @export
#' @md
#' @seealso \code{\link{extract_yhat}}  \code{\link[forecast]{simulate.Arima}} \code{\link{arima_tidy}}
#'
#' @examples
extract_yhat.Arima <- function(fit, newdata, resp_var, newxreg = NULL, ...) {
  # We have to assume that the rows of newdata/newxreg are in order from earliest to latest.

  #I'm not 100% sure if this should be data masking or tidy-selecting? so either select or transmute.
  # I think if we use transmute, we can match anything that's in the formula. So I should do that.
  browser()
  yvar <- dplyr::transmute(newdata, {{resp_var}})[[1]]
  n_old <- length(fit$residuals)
  old_fitted <- yvar[1:n_old] - fit$residuals
  n_new <- nrow(newdata)
  n_ahead <- n_new - n_old
  new_fitted <- predict(fit, n_ahead, newxreg = newxreg[n_old + seq_len(n_ahead), , drop = FALSE])
  comb_fitted <- c(as.numeric(old_fitted), as.numeric(new_fitted$pred))
  newdata$.fitted <- comb_fitted
  newdata$.resid <- yvar - comb_fitted
  newdata
}


#' @rdname extract_yhat.Arima
extract_yhat.forecast_ARIMA <- function(fit, newdata, newxreg = NULL, ...) {
  n_ahead <- nrow(newdata) - length(fit$fitted)
  if (missing(newxreg) || is.null(newxreg)) {
    new_predictions <- as.numeric(
      forecast::forecast(fit, h = n_ahead)$mean
    )
  } else {
    rows_to_use <- seq(nrow(newxreg) - n_ahead + 1, nrow(newxreg))
    new_predictions <- as.numeric(
      forecast::forecast(fit, h = n_ahead, xreg = newxreg[rows_to_use, , drop = FALSE])$mean
    )
  }
  old_predictions <- as.numeric(fit$fitted)
  predictions <- c(old_predictions, new_predictions)
  newdata$.fitted <- predictions
  return(newdata)
}


