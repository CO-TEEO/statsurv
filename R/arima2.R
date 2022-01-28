#' Title
#'
#' @param f
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
arima2 <- function(f, data, ...) {
  xreg <- gen_xreg(f, data)
  resp <- gen_resp(f, data)
  fit <- arima(resp, xreg = xreg, ...)
  fit$formula <- f
  fit$data <- data
  fit$fitted <- resp - fit$residuals
  class(fit) <- c("arima2", class(fit))
  fit
}


augment.arima2 <- function(fit, newdata, ...) {

  n_ahead <- nrow(newdata) - length(fit$fitted)
  new_xreg <- gen_xreg(fit$formula, newdata)
  # What's the cleanest way to get only the new rows?
  # Is it an anti-join?
  # Or is it
  vars <- all.vars(fit$formula)
  old_data <- fit$data %>%
    dplyr::select(dplyr::all_of(vars))
  new_data <- newdata %>%
    dplyr::select(dplyr::all_of(vars))
  newnew_data <- dplyr::anti_join(new_data, old_data, by = vars)
  xreg <- gen_xreg(fit$formula, newnew_data)
  new_predictions <- as.numeric(stats::predict(fit, n.ahead = n_ahead, newxreg = xreg)$pred)
  old_predictions <- as.numeric(fit$fitted)
  predictions <- c(old_predictions, new_predictions) #I don't know if we can assume this
  newdata$.fitted <- predictions
  return(newdata)
}


gen_xreg <- function(f, data) {
  matrix <- model.matrix(f, data = data)
  m <- colnames(matrix) != "(Intercept)"
  matrix[, m, drop = FALSE]
}

gen_resp <- function(f, data) {
  model.response(model.frame(f, data = data))
}
