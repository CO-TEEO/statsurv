
make_matrix <- function(f, df) {
  f[[2]] <- rlang::sym("y_null")
  matrix <- model.matrix(f, data = df)
  m <- colnames(matrix) != "(Intercept)"
  matrix[, m, drop = FALSE]
}

check_arima <- function(space, time, fit, data, xreg, n) {
  res <- extract_yhat(space, time, fit, data = data, xreg = xreg, n_ahead = length(n))
  expect_equal(as.numeric(fit$fitted[-n]),
               res$yhat[-n])
  if (missing(xreg)) {
    expect_equal(as.numeric(forecast::forecast(fit, h = length(n))$mean),
                 res$yhat[n])
  } else {
    expect_equal(as.numeric(forecast::forecast(fit, xreg = xreg[n, , drop = FALSE])$mean),
                 res$yhat[n])
  }
  expect_true("data.frame" %in% class(res))
}


# ARIMA models behave pretty differently than other models.
# Currently they have to be uni-ariate, and NAs aren't ignored in fitting.
# So we basically need to create a new set of data.
ac_space <- data.frame(space = "space1",
                       stringsAsFactors = FALSE)
ac_time <- generate_date_range(lubridate::ymd("2010-01-01"),
                               lubridate::ymd("2019-12-21"),
                               time_division = "month")
comb_coords <- gridcoord::gc_expand(ac_space, ac_time)

n <- nrow(comb_coords)
na_inds <- 1:10
good_inds <- seq_len(n)[-na_inds]

x_continuous <- runif(n, min = -5, max = 10) #continuous predictor
nu <- as.numeric(arima.sim(list(ar = c(0.9, -0.2)), n = n))
y_ac1 <- 3 + 2.5 * x_continuous + nu
ac_data <- cbind(comb_coords, x_continuous, y_ac1, y_null = 1)

test_that("ARIMA model (1, 0, 0), n_ahead = 1", {
  skip_if_not(forecast_available)
  xreg <- make_matrix(y_ac1 ~ x_continuous, ac_data)
  fit_arima <- forecast::Arima(ac_data$y_ac1[-n],
                               order = c(1,0,0),
                               method = "ML",
                               include.mean = TRUE, # include intercept
                               transform.pars = FALSE,
                               xreg = xreg[-n, , drop = FALSE])
  check_arima(ac_space, ac_time, fit_arima, ac_data, xreg, 120)
})

test_that("ARIMA model (2, 1, 0), n_ahead = 1", {
  skip_if_not(forecast_available)
  xreg <- make_matrix(y_ac1 ~ x_continuous, ac_data)
  fit_arima <- forecast::Arima(ac_data$y_ac1[-n],
                               order = c(2,1,0),
                               method = "ML",
                               include.mean = TRUE, # include intercept
                               transform.pars = FALSE,
                               xreg = xreg[-n, , drop = FALSE])
  check_arima(ac_space, ac_time, fit_arima, ac_data, xreg, 120)
})

test_that("ARIMA include.mean = FALSE", {
  skip_if_not(forecast_available)
  xreg <- make_matrix(y_ac1 ~ x_continuous, ac_data)
  fit_arima <- forecast::Arima(ac_data$y_ac1[-n],
                               order = c(2,1,0),
                               method = "ML",
                               include.mean = FALSE,
                               transform.pars = FALSE,
                               xreg = xreg[-n, , drop = FALSE])
  check_arima(ac_space, ac_time, fit_arima, ac_data, xreg, 120)
})

test_that("ARIMA transform.pars = TRUE", {
  skip_if_not(forecast_available)
  xreg <- make_matrix(y_ac1 ~ x_continuous, ac_data)
  fit_arima <- forecast::Arima(ac_data$y_ac1[-n],
                               order = c(2,1,0),
                               method = "ML",
                               include.mean = FALSE,
                               transform.pars = TRUE,
                               xreg = xreg[-n, , drop = FALSE])
  check_arima(ac_space, ac_time, fit_arima, ac_data, xreg, 120)
})

test_that("ARIMA include.drift = TRUE", {
  skip_if_not(forecast_available)
  xreg <- make_matrix(y_ac1 ~ x_continuous, ac_data)
  fit_arima <- forecast::Arima(ac_data$y_ac1[-n],
                               order = c(2,1,0),
                               method = "ML",
                               include.mean = FALSE,
                               include.drift = TRUE,
                               transform.pars = FALSE,
                               xreg = xreg[-n, , drop = FALSE])
  check_arima(ac_space, ac_time, fit_arima, ac_data, xreg, 120)
})

test_that("ARIMA include.drift = TRUE", {
  skip_if_not(forecast_available)
  xreg <- make_matrix(y_ac1 ~ x_continuous, ac_data)
  fit_arima <- forecast::Arima(ac_data$y_ac1[-n],
                               order = c(2,1,0),
                               method = "ML",
                               include.mean = FALSE,
                               include.drift = TRUE,
                               transform.pars = FALSE,
                               xreg = xreg[-n, , drop = FALSE])
  check_arima(ac_space, ac_time, fit_arima, ac_data, xreg, 120)
})


test_that("ARIMA method = 'CSS'", {
  skip_if_not(forecast_available)
  xreg <- make_matrix(y_ac1 ~ x_continuous, ac_data)
  fit_arima <- forecast::Arima(ac_data$y_ac1[-n],
                               order = c(2,1,0),
                               method = "CSS",
                               include.mean = FALSE,
                               include.drift = TRUE,
                               transform.pars = FALSE,
                               xreg = xreg[-n, , drop = FALSE])
  check_arima(ac_space, ac_time, fit_arima, ac_data, xreg, 120)
})

test_that("ARIMA method = 'CSS-ML'", {
  skip_if_not(forecast_available)
  xreg <- make_matrix(y_ac1 ~ x_continuous, ac_data)
  fit_arima <- forecast::Arima(ac_data$y_ac1[-n],
                               order = c(2,1,0),
                               method = "CSS-ML",
                               include.mean = TRUE,
                               include.drift = FALSE,
                               transform.pars = TRUE,
                               xreg = xreg[-n, , drop = FALSE])
  check_arima(ac_space, ac_time, fit_arima, ac_data, xreg, 120)
})

test_that("ARIMA, no xreg", {
  skip_if_not(forecast_available)
  fit_arima <- forecast::Arima(ac_data$y_ac1[-n],
                               order = c(1, 0, 0),
                               )
  check_arima(ac_space, ac_time, fit_arima, ac_data, n = 120)

  n_pred <- 118:120
  fit_arima <- forecast::Arima(ac_data$y_ac1[-n_pred],
                               order = c(1, 0, 0),
  )
  check_arima(ac_space, ac_time, fit_arima, ac_data, n = n_pred)

})



test_that("regular Arima throws an error", {
  fit_arima <- arima(ac_data$y_ac1[-n],
                     order = c(1, 0, 0))
  expect_error(extract_yhat(ac_space, ac_time, fit_arima, ac_data),
               "forecast::Arima")

  expect_error(sample_yhat(ac_space, ac_time, fit_arima, ac_data),
               "Arima")

})

test_that("forecast::Arima throws an error on multiariate data", {
  fit_arima <- forecast::Arima(ey_data$y_lm[-n],
                               order = c(1, 0, 0))

  expect_error(extract_yhat(ey_space, ey_time, fit_arima, ey_data),
               "ariate")
})
