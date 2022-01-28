id_time <- seq(1:100)
d <- rnorm(n = length(id_time))
y <- cumsum(d)
arima_data <- tibble::tibble(id_time = id_time, id_space = 1, y = y)
knot <- 42
arima_data <- arima_data %>%
  dplyr::mutate(yp = y + ifelse(id_time > knot, (id_time - knot) * 0.2, 0))

make_matrix <- function(f, df) {
  f[[2]] <- rlang::sym("y_null")
  matrix <- model.matrix(f, data = df)
  m <- colnames(matrix) != "(Intercept)"
  matrix[, m, drop = FALSE]
}

make_matrix(y ~ id_time + pmax(id_time-36, 0), df = arima_data)
model.matrix(y ~ id_time + pmax(id_time-36, 0), data = arima_data)

# WOrking prototype for gen_xreg
gen_xreg <- function(f, data) {
  matrix <- model.matrix(f, data = data)
  m <- colnames(matrix) != "(Intercept)"
  matrix[, m, drop = FALSE]
}

gen_resp <- function(f, data) {
  model.response(model.frame(f, data = data))
}

xreg <- gen_xreg(y ~ id_time + pmax(id_time - 36, 0), arima_data)


# Now playing with the prepare_prediction_xreg
# Hrm. Not sure that would actually work.
# What if we wrote our own wrapper?

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

input_data <- prepare_prediction_data(arima_data, outcome_cols = y, split_id = 100, prep_strategy = "truncate")
input_xreg <- gen_xreg(y ~ id_time + pmax(id_time - 36, 0), input_data)
arima_xreg <- gen_xreg(y ~ id_time + pmax(id_time - 36, 0), arima_data)
fit <- arima2(y ~ id_time + pmax(id_time - 36, 0), input_data, order = c(1,1,0), include.mean = TRUE)
fit_base <- arima(input_data$y, order = c(1, 1, 0), xreg = input_xreg, include.mean = TRUE)
fit_fore <- forecast::Arima(input_data$y, order = c(1, 1, 0), xreg = input_xreg, include.mean = TRUE)
extract_yhat(fit, arima_data)
# Hey! I think that works! That's awesome!
extract_yhat.arima2 <- function(fit, newdata) {

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

  if (missing(xreg)) {
    new_predictions <- as.numeric(
      forecast::forecast(fit, h = n_ahead)$mean
    )
  } else {
    rows_to_use <- seq(nrow(xreg) - n_ahead + 1, nrow(xreg))
    new_xreg <- xreg[rows_to_use, , drop = FALSE]
    new_predictions <- as.numeric(
      forecast::forecast(fit, h = n_ahead, xreg = new_xreg)$mean
    )
  }
  old_predictions <- as.numeric(fit$fitted)
  predictions <- c(old_predictions, new_predictions)
  data$.fitted <- predictions
  return(data)
}

# Ok, can I get it to work for stats::arima?
f <- function(fit, newdata, resp_var, newxreg) {

  # Ok. I guess that we just assume that we only want the last n rows of newxreg
  # But how do we calculate the fitted? I don't think we can - we have the reisudals, but not
  # the associated data sets. I think that's probably a deal breaker.

  # So, I think we don't have this use stats::arima.
  # And we point people to either arima2 or forecast_Arima
  # It like...almost works. But not quite.
  # No wait...can't we use the first n rows of newdata?
  # Ah, no, not necessarily, because I need to know which column.
  # But that's not actually a problem, because we can just add extra inputs.
  yvar <- dplyr::select(newdata, {{resp_var}})[[1]] #Is that reasonable? I'm not 100% sure if this should be data masking or tidy-selecting? so either select or transmute.
  n_old <- length(fit$residuals)
  old_fitted <- yvar[1:n_old] - fit$residuals
  n_new <- nrow(newdata)
  n_ahead <- n_new - n_old
  new_fitted <- predict(fit, n_ahead, newxreg = newxreg[n_old + seq_len(n_ahead), , drop = FALSE])
  comb_fitted <- c(as.numeric(old_fitted), as.numeric(new_fitted$pred))
  newdata$.fitted <- comb_fitted
  newdata
}

res <- f(fit_base, arima_data, y, arima_xreg)
library(ggplot2)
ggplot(res, aes(x = id_time, y = y)) +
  geom_line() +
  geom_line(aes(y = .fitted))
# That looks...too good?

z <- extract_yhat.forecast_ARIMA(fit_fore, arima_data, arima_xreg)
z
res
