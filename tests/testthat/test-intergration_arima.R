# This is an experiment. It's less about checking for correctness, and more checking for no errors
suppressWarnings(library("lubridate"))
suppressWarnings(library("forecast"))
suppressWarnings(library("dplyr"))

# source(here::here("tests", "testthat", "setup-cdc_se_calc.R"))

test_that("Integration test for ARIMA + HSR", {
  skip_on_cran()
  skip_if_not_installed("forecast")


  create_xreg <- function(spacetime_data) {
    df <- spacetime_data

    xreg <- as.matrix(x = df[, c("offset", "months_since_4", "x")])
    return(xreg)
  }
  create_xreg <<- create_xreg

  model_arima <- function(spacetime_data, xreg) {
    library("forecast")


    fit_arima <- forecast::Arima(spacetime_data$y,
                                 order = c(1,0,0),
                                 method = "ML",
                                 include.mean = FALSE,
                                 transform.pars = FALSE,
                                 xreg = xreg)

    return(fit_arima)
  }
  model_arima <<- model_arima

  spacetime_data <- expand.grid(id_space = 1:5,
                                id_time = 1:48) %>%
    dplyr::mutate(offset = runif(n = 5, max = 30)[id_space],
                  months_since_4 = pmax(id_time - 4, 0),
                  x = rnorm(mean = 6, sd = 3, n = dplyr::n()))

  # Split, generate y
  gen_y <- function(df) {
    n <- nrow(df)
    nu <- as.numeric(arima.sim(list(ar = c(0.9, -0.2)), n = n))
    df$y <- 3 + 2.5 * df$x + df$offset + -0.3 * df$months_since_4 + nu
    df
  }
  spacetime_data <- split(spacetime_data, spacetime_data$id_space) %>%
    lapply(gen_y) %>%
    do.call(what = rbind)
  rownames(spacetime_data) <- NULL

  windowed_data <- spacetime_data %>%
    window_idtime(min_train = 7, max_train = 24,
                  split_spatial_locations = TRUE,
                  n_predict = 1) %>%
    rowmute(training_data = prepare_training_data(curr_data, y, split_id, "truncate"),
            xreg = create_xreg(training_data))
  fitted_results <- windowed_data %>%
    rowmute(arima_fit = model_arima(training_data, xreg))


  predictions <- fitted_results %>%
    rowmute(xreg2 = create_xreg(curr_data)) %>%
    rowmute(aug_data = extract_yhat(arima_fit, curr_data, xreg2))




  pred_w_err <- predictions %>%
    rowmute(v = sqrt(median(compute_arima_se(arima_fit, add_intercept = FALSE))))

  collapsed_pred_w_err <- pred_w_err %>%
    group_by(window_time_id) %>%
    collapse_all(unlist_scalars = TRUE) %>%
    rowmute(avg_v = mean(v))

  all_scanres <-
    collapsed_pred_w_err %>%
    rowmute(alarm_res = parallel_cusum_gaussian2(aug_data, outcome_col = y, baseline_col = .fitted,
                                                 sigma = avg_v, drift = 1))




  big_df <- all_scanres %>%
    ungroup() %>%
    select(window_time_id, alarm_res) %>%
    tidyr::unnest(alarm_res)
  surv_df <- big_df %>%
    group_by(window_time_id) %>%
    slice_max(id_time)

  expect_true(is.list(all_scanres$alarm_res))
  expect_true(is.data.frame(surv_df))
})
