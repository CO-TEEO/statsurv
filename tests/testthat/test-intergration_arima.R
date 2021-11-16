# This is an experiment. It's less about checking for correctness, and more checking for no errors
suppressWarnings(library("lubridate"))
suppressWarnings(library("forecast"))

test_that("Integration test for ARIMA + HSR", {
  skip_on_cran()
  skip_if_not(forecast_available)



  space_coord <- generate_study_area(space_division = "co_hsr_2019")[1:5, ]
  time_coord <- generate_date_range(start_date = ymd("2016-01-01"),
                                    end_date = ymd("2019-12-31"),
                                    "month")

  # Ok, now we have to come up with data.
  # There should be some covriates that vary with time, there should be an overall drift.

  space_data <- data.frame(hsr = space_coord$hsr,
                           stringsAsFactors = FALSE)
  space_data$offset <- runif(nrow(space_data), max = 30)

  time_data <- data.frame(date_label = time_coord[[1]],
                          stringsAsFactors = FALSE)
  time_data$months_since_14 <- seq_len(nrow(time_data))

  data <- gridcoord::gc_expand(space_data, time_coord) %>%
    dplyr::left_join(y = time_data, by = "date_label")
  data$x <- rnorm(mean = 6, sd = 3, n = nrow(data))

  # Split, generate y
  gen_y <- function(df) {
    n <- nrow(df)
    nu <- as.numeric(arima.sim(list(ar = c(0.9, -0.2)), n = n))
    df$y <- 3 + 2.5 * df$x + df$offset + -0.3 * df$months_since_14 + nu
    df
  }
  data <- split(data, data$hsr) %>%
    lapply(gen_y) %>%
    do.call(what = rbind)
  rownames(data) <- NULL

  fits_and_data <- loop_model(space_coord, time_coord, data, "y",
                              path_to_model = "model_arima.R",
                              model_arity = "uni", use_cache = FALSE)

  all_fits <- fits_and_data[[1]]
  all_data <- fits_and_data[[2]]
  all_xreg <- fits_and_data[[3]]

  all_yhat <- loop_extract_yhat(space_coord, time_coord,
                                all_fits, all_data, "extract",
                                path_to_model = "model_arima.R",
                                use_cache = FALSE,
                                extra_extractor_args = list(xreg = all_xreg, n_ahead = 1))


  source("cdc_se_calc.R")
  all_v <- loop_over(space_coord, time_coord,
                     all_fits,
                     function(x) sqrt(median(compute_arima_se(x, add_intercept = FALSE))),
                     title = "Calculating model errors")
  flat_v <- lapply(gridcoord::gcl_collapse(all_v, "space", return_gridlist = FALSE),
                   function(x) mean(x$values))
  all_scanres <- loop_alarm_function(space_coord, time_coord,
                                     list_of_yhats = all_yhat,
                                     list_of_model_data = all_data,
                                     outcome_col = "y",
                                     alarm_function_name = "parallel_cusum_gaussian",
                                     path_to_model = "model_arima.R",
                                     extra_alarm_args = list(sigma = flat_v, drift = 1))

  expect_true(is.list(all_scanres))
  unlink("cache_alarm", recursive = TRUE)
})
