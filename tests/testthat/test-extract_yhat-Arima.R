make_matrix <- function(f, df) {
  f[[2]] <- rlang::sym("y_null")
  matrix <- model.matrix(f, data = df)
  m <- colnames(matrix) != "(Intercept)"
  matrix[, m, drop = FALSE]
}


# Ok, what should we check? That extract_yhat matches predict or forecast, as appropriate.
# Also some checks that our results are reasonable
# I guess that extract_yhat.Arima matches extract_what.forecast_Arima for the first n variables.
check_arima <- function(fit, newdata, newxreg, resp_var, n) {
  res <- extract_yhat(fit, newdata = newdata, newxreg = newxreg,
                      resp_var = resp_var)
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
arima_data <- tibble::tibble(id_time = 1:100, id_space = 1)

n <- nrow(arima_data)
na_inds <- which(arima_data$id_time >= 98)
good_inds <- seq_len(n)[-na_inds]

x_continuous <- runif(n, min = -5, max = 10) #continuous predictor
nu <- as.numeric(arima.sim(list(ar = c(0.9, -0.2)), n = n))
y_ac1 <- 3 + 2.5 * x_continuous + nu
x_discrete <- sample(c("L1", "L2", "L3"), n, replace = TRUE) #discrete predictor
factor_coeffs <- c(-2, 3, 1) %>%
  magrittr::set_names(sort(unique(x_discrete)))
y_ac2 <- 3 + 2.5 * x_continuous + factor_coeffs[x_discrete] + nu
ac_newdata <- cbind(arima_data, x_continuous, x_discrete, y_ac1, y_ac2)
ac_data <- ac_newdata[good_inds, ]

newxreg1 <- ac_newdata %>%
  dplyr::select(x_continuous) %>%
  as.matrix()

newxreg2 <- ac_newdata %>%
  dplyr::transmute(x_continuous, (x_discrete == "L2")*1, (x_discrete == "L3")*1) %>%
  as.matrix()

xreg1 <- newxreg1[good_inds, , drop = FALSE]
xreg2 <- newxreg2[good_inds, , drop = FALSE]

all_xregs <- list(xreg0 = NULL, xreg1 = xreg1, xreg2 = xreg2)
all_newxregs <- list(xreg0 = NULL, xreg1 = newxreg1, xreg2 = newxreg2)

# Repeat for the formulas used in arima_tidy
f0 <- y_ac1 ~ 1
f1 <- y_ac1 ~ x_continuous
f2 <- y_ac2 ~ x_continuous + factor(x_discrete)
all_formulas <- list(xreg0 = f0, xreg1 = f1, xreg2 = f2)

# Test it out on lots of different possible types of inputs
arima_combos <-
  expand.grid(xreg = c("xreg0", "xreg1", "xreg2"),
            seasonal_order = list(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0)),
            seasonal_period = 12,
            include.mean = c(TRUE, FALSE),
            order = list(c(1, 0, 0), c(2, 1, 0)),
            stringsAsFactors = FALSE) %>%
  tibble::as_tibble()

for (ind in seq_len(nrow(arima_combos))) {
  curr_row <- arima_combos[ind, ]
  if (curr_row$xreg == "xreg2") {
    yname <- "y_ac2"
    y <- ac_data$y_ac2
  } else {
    yname <- "y_ac1"
    y <- ac_data$y_ac1
  }
  # I know this is bad practice, but stats::predict.Arima pulls these values out of the
  # parent environment, and the environments in testthat are weird so we have to set them
  # manually
  rlang::env_poke(rlang::global_env(), nm = "all_xregs", all_xregs)
  rlang::env_poke(rlang::global_env(), nm = "curr_row", curr_row)


  fit_stats <- stats::arima(ac_data[[yname]],
                            order = curr_row$order[[1]],
                            seasonal = list(order = curr_row$seasonal_order[[1]],
                                            period = curr_row$seasonal_period),
                            xreg = all_xregs[[curr_row$xreg]],
                            include.mean = curr_row$include.mean)

  yhat_stats <- extract_yhat(fit_stats, newdata = ac_newdata, resp_var = .data[[yname]],
               newxreg = all_newxregs[[curr_row$xreg]])

  fit_forecast <- forecast::Arima(ac_data[[yname]],
                                  order = curr_row$order[[1]],
                                  seasonal = list(order = curr_row$seasonal_order[[1]],
                                                  period = curr_row$seasonal_period),
                                  xreg = all_xregs[[curr_row$xreg]],
                                  include.mean = curr_row$include.mean)

  yhat_forecast <- extract_yhat(fit_forecast, newdata = ac_newdata, newxreg = all_newxregs[[curr_row$xreg]])
  test_that("stats and forecast give the same results", {
    expect_equal(dplyr::select(yhat_stats, -.resid), yhat_forecast)
  })
  test_that("stats gives reasonable results", {
    expect_true(is.data.frame(yhat_stats))
    expect_true(".fitted" %in% colnames(yhat_stats))
  })
  test_that("forecast numbers are what we'd expect", {
    expect_equal(as.numeric(fit_forecast$fitted[good_inds]),
                 yhat_forecast$.fitted[good_inds])
    curr_newxreg <- all_newxregs[[curr_row$xreg]]
    if (is.null(curr_newxreg)) {
      pred <- forecast::forecast(fit_forecast, h = length(na_inds))$mean
    } else {
      pred <- forecast::forecast(fit_forecast, xreg = curr_newxreg[na_inds, , drop = FALSE])$mean
    }
    expect_equal(as.numeric(pred),
                 yhat_forecast$.fitted[na_inds])
  })
  fit_arima_tidy <- arima_tidy(all_formulas[[curr_row$xreg]],
                               data = ac_data,
                               order = curr_row$order[[1]],
                               seasonal = list(order = curr_row$seasonal_order[[1]],
                                               period = curr_row$seasonal_period),
                               include.mean = curr_row$include.mean)
  yhat_arima_tidy <- extract_yhat(fit_arima_tidy, ac_newdata)
  test_that("arima_tidy matches arima", {
    expect_equal(yhat_arima_tidy, yhat_stats)
    expect_equal(fit_arima_tidy$model, fit_stats$model)
  })
}

test_that("Works if n.ahead = 0", {
  fit_stats_arima <- stats::arima(ac_newdata$y_ac2, order = c(2, 0, 0), xreg = newxreg2)
  yhat_stats_arima <- extract_yhat(fit_stats_arima, ac_newdata, y_ac2)

  fit_forecast <- forecast::Arima(ac_newdata$y_ac2, order = c(2, 0, 0), xreg = newxreg2)
  yhat_forecast <- extract_yhat(fit_forecast, ac_newdata)

  fit_arima_tidy <- arima_tidy(f2, data = ac_newdata, order = c(2, 0, 0))
  yhat_arima_tidy <- extract_yhat(fit_arima_tidy, ac_newdata)

  expect_equal(yhat_forecast$.fitted, as.numeric(fit_forecast$fitted))
  expect_equal(yhat_forecast, dplyr::select(yhat_stats_arima, -.resid))
  expect_equal(yhat_forecast, dplyr::select(yhat_arima_tidy, -.resid))
})

