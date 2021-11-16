set.seed(232892630)

space_coord <- rgdal::readOGR("three_zips/three_zips.shp",
                              verbose = FALSE,
                              stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()

start_times <- 1:12
fin_times <- start_times + 1L
labels <- paste0("X", start_times) #sapply(start_times + 64, intToUtf8)
time_coord <- data.frame(time_labels = labels,
                         time = start_times,
                         fin_time = fin_times,
                         stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()

data_for_scan <- gridcoord::gc_expand(time_coord, space_coord)
data_for_scan$baseline <- 4.3
data_for_scan$baseline2 <- 6
# is_outbreak <- #space2 and space3, 10-13
is_outbreak <- data_for_scan$zcta_str %in% c("80401", "80203") & data_for_scan$time >= 9
data_for_scan$observed <- floor(data_for_scan$baseline + ifelse(is_outbreak, 4, 0))
null_f <- function(space_coord, time_coord, data_for_model) {
  return(list(fit = 0, data = data_for_model))
}
all_ret <- loop_model(space_coord, time_coord, data_for_scan, "observed", null_f, use_cache = FALSE)
all_data_for_scan <- all_ret[[2]]
all_yhats <- lapply(all_data_for_scan,
                    function(x) {
                      if (identical(x, NA)) {
                        x
                       } else {
                          x[, c("time_labels", "zcta_str", "baseline", "baseline2")]
                        }
                      })

calculate_ebp_scores <- function(n_z, mu_z) {
  ebp_poisson_score <- n_z * log(n_z / mu_z) + mu_z - n_z
  ebp_poisson_score[n_z < mu_z] <- 0
  return(ebp_poisson_score)
}

test_that("loop_alarm_function runs", {
  expect_error(loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                   "scan_eb_poisson", max_k = 1, use_cache = FALSE),
               NA)
})

# We can at least check the scores
test_that("loop_alarm_function works?", {
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                  "scan_eb_poisson", path_to_model = "test_path", max_k = 1,
                                 use_cache = FALSE, verbose = FALSE)
  all_scan <- all_res
  sorted_output <- all_scan$X11$observed %>%
    dplyr::arrange(zone, duration)
  output_to_check <- sorted_output[sorted_output$duration == 1, ]
  input_data <- all_data_for_scan$X11[all_data_for_scan$X11$time_labels == "X11", ]
  expected_scores1 <- calculate_ebp_scores(input_data$observed, input_data$baseline)
  expected_scores2 <- calculate_ebp_scores(input_data$observed, input_data$baseline2)
  expected_scores_avg <- 0.5 * (expected_scores1 + expected_scores2)
  expect_equal(output_to_check$action_level, expected_scores_avg)
})

test_that("loop_alarm_function is consistent", {
  ### scan_eb_poisson
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_eb_poisson", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE)
  expect_known_value(all_res, "scanstat/scan_eb_poisson.RDS")

  ### scan_pb_poisson
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_pb_poisson", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE)
  expect_known_value(all_res, "scanstat/scan_pb_poisson.RDS")

  ### scan_eb_negbin
  thetas <- list(0.1, 0.5, 1, 5, 10)
  names(thetas) <- names(all_yhats)[!is.na(all_yhats)]
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_eb_negbin", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE,
                                 extra_alarm_args = list(thetas = thetas))

  expect_known_value(all_res, "scanstat/scan_eb_negbin.RDS")
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_eb_negbin", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE)
  expect_known_value(all_res, "scanstat/scan_eb_negbin_v2.RDS")

  ### scan_eb_zip
  all_res <- suppressWarnings(loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_eb_zip", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE))
  expect_known_value(all_res, "scanstat/scan_eb_zip.RDS")
  make_probs <- function(data, value, column_coord, row_coord) {
    data[[".value."]] <- value
    return(pivot_for_scan(data, ".value.", column_coord, row_coord))
  }
  prob_vals <- list(0.1, 0.2, 0.3, 0.4, 0.5)
  names(prob_vals) <- names(all_data_for_scan)[!is.na(all_data_for_scan)]
  extra_args <- list(value = prob_vals,
                     column_coord = space_coord,
                     row_coord = time_coord)
  all_probs <- loop_over(space_coord, time_coord,
                         all_data_for_scan, make_probs, extra_args = extra_args)
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_eb_zip", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE,
                                 extra_alarm_args = list(probs = all_probs))
  expect_known_value(all_res, "scanstat/scan_eb_zip_v2.RDS")

  ### scan_permutation
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_permutation", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE)
  expect_known_value(all_res, "scanstat/scan_permutation.RDS")

  ### scan_bayes_negbin
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_bayes_negbin", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE)
  expect_known_value(all_res, "scanstat/scan_bayes_negbin.RDS")
  all_beta <- list(0.1, 0.3, 0.5, 0.7, 1)
  names(all_beta) <- names(all_data_for_scan)[!is.na(all_data_for_scan)]
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_bayes_negbin", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE,
                                 extra_alarm_args = list(beta_null = all_beta))
  expect_known_value(all_res, "scanstat/scan_bayes_negbin_v2.RDS")
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_bayes_negbin", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE,
                                 extra_alarm_args = list(beta_null = 0.6))
  expect_known_value(all_res, "scanstat/scan_bayes_negbin_v3.RDS")

  ### scan_eb_poisson_fast
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_eb_poisson_fast", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE)
  expect_known_value(all_res, "scanstat/scan_eb_poisson_fast.RDS")

  ### scan_cusum_poisson
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_cusum_poisson", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE)
  expect_known_value(all_res, "scanstat/scan_cusum_poisson.RDS")

  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_cusum_poisson", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE,
                                 extra_alarm_args = list(scaling = 2))
  expect_known_value(all_res, "scanstat/scan_cusum_poisson_v2.RDS")
  scalings <- list(1.1, 1.1, 1.3, 1.5, 3)
  names(scalings) <- names(all_data_for_scan)[!is.na(all_data_for_scan)]
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "scan_cusum_poisson", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE,
                                 extra_alarm_args = list(scaling = scalings))
  expect_known_value(all_res, "scanstat/scan_cusum_poisson_v3.RDS")

  ### parallel_cusum_poisson
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "parallel_cusum_poisson", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE)
  expect_known_value(all_res, "scanstat/parallel_cusum_poisson.RDS")

  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "parallel_cusum_poisson", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE,
                                 extra_alarm_args = list(scaling = 2))
  expect_known_value(all_res, "scanstat/parallel_cusum_poisson_v2.RDS")
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "parallel_cusum_poisson", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE,
                                 extra_alarm_args = list(scaling = scalings))
  expect_known_value(all_res, "scanstat/parallel_cusum_poisson_v3.RDS")

  ### parallel_cusum_gaussian
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "parallel_cusum_gaussian", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE)
  expect_known_value(all_res, "scanstat/parallel_cusum_gaussian.RDS")

  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "parallel_cusum_gaussian", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE,
                                 extra_alarm_args = list(drift = 0.75))
  expect_known_value(all_res, "scanstat/parallel_cusum_gaussian_v2.RDS")
  all_res <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                 "parallel_cusum_gaussian", path_to_model = "test_path", max_k = 3,
                                 use_cache = FALSE, verbose = FALSE,
                                 extra_alarm_args = list(drift = 0.75,
                                                        sigma = scalings))
  expect_known_value(all_res, "scanstat/parallel_cusum_gaussian_v3.RDS")
})

test_that("No warnings on scan_eb_poisson_fast", {
  # Note - this seems to be random error at the moment, so that's fun
  all_bad_obs <- lapply(all_data_for_scan,
                        function(x) {
                          if (identical(x, NA)) {
                            x
                          } else {
                            x$observed <- 1000
                            x
                          }
                        })
  expect_warning(loop_alarm_function(space_coord, time_coord, all_yhats, all_bad_obs, "observed",
                                     "scan_eb_poisson_fast", path_to_model = "test_path", max_k = 3,
                                     n_mcsim = 100, force = TRUE),
                 NA)
  unlink("cache_alarm", recursive = TRUE)
})



