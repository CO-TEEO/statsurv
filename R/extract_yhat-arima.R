
#' @export
extract_yhat.Arima <- function(fit, newdata, newxreg, resp_var, ...) {
  # We have to assume that the rows of newdata/newxreg are in order from earliest to latest.

  #I'm not 100% sure if this should be data masking or tidy-selecting? so either select or transmute.
  yvar <- dplyr::select(newdata, {{resp_var}})[[1]]
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

#' @title Extract fitted values from a forecast_ARIMA model object
#'
#'
#' @param xreg A matrix of covariate information used to calculate ARIMA values. Must cover the
#'   exact same temporal and spatial locations as `data`
#' @param n_ahead How many data points should be predicted?
#' @param ... Other arguments
#' @inheritParams extract_yhat
#'
#' @details ARIMA models currently only have limited support in the statsurv package. The syntax for
#'   fitting and working with ARIMA models is significantly different than that of `lm` or `glm`
#'   models. Some of the key differences are:
#' \enumerate{
#'   \item extract_yhat.forecast_ARIMA only works on models containing a single time series. In
#'   other words, the space coordinate must only have a single location.
#'   \item forecast_ARIMA models do not use the `data` parameter, instead any covariates must be
#'   included in the matrix `xreg`
#'   \item extract_yhat.forecast_ARIMA needs to know how many time-steps ahead should be predicted,
#'   as specified by the `n_ahead` parameter.
#'   }
#' @inherit extract_yhat return
#' @seealso \code{\link{extract_yhat}}, \code{\link[forecast]{forecast}}
#' @export
#' @md
#' @examples
#' \dontrun{
#' library("scanstatistics")
#' library("forecast")
#' nm_county_coord <- statsurv::nm_county_coord
#' data(NM_popcas)
#' year_coord <- generate_date_range(lubridate::ymd("1973-01-01"),
#'                                   lubridate::ymd("1991-01-01"),
#'                                   time_division = "year")
#' year_coord$year <- year_coord$date_label
#' year_coord <- gridcoord::gc_gridcoord(year_coord, "year")
#'
#' # Fit the data for Santa Fe county via an ARIMA model,
#' # not including the last 2 data points:
#' santa_fe <- NM_popcas %>%
#'   dplyr::filter(county == "santafe")
#' sf_coord <- nm_county_coord %>%
#'   dplyr::filter(county == "santafe")
#' n <- seq(1, nrow(santa_fe) - 2)
#' xreg = as.matrix(santa_fe[n, "population", drop = FALSE])
#' fit_Arima <- Arima(santa_fe$count[n],
#'                    order = c(2, 1, 0),
#'                    xreg = xreg[n, , drop = FALSE])
#'
#' # Then use extract_yhat to generate predictions:
#' # Because we did not include the last 2 points in fitting the model,
#' # we set n_ahead = 2
#' extract_yhat(sf_coord, year_coord, fit_Arima, santa_fe,
#'              xreg, n_ahead = 2)
#' }
extract_yhat.forecast_ARIMA <- function(fit, newdata, newxreg) {
  n_ahead <- nrow(newdata) - length(fit$fitted)
  if (missing(newxreg)) {
    new_predictions <- as.numeric(
      forecast::forecast(fit, h = n_ahead)$mean
    )
  } else {
    rows_to_use <- seq(nrow(newxreg) - n_ahead + 1, nrow(xreg))
    new_predictions <- as.numeric(
      forecast::forecast(fit, h = n_ahead, xreg = newxreg[rows_to_use, , drop = FALSE])$mean
    )
  }
  old_predictions <- as.numeric(fit$fitted)
  predictions <- c(old_predictions, new_predictions)
  data$.fitted <- predictions
  return(data)
}


