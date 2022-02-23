library(scanstatistics)
library(here)
# source(here("tests", "testthat", "setup-alarm_function_data.R"))
# Gets us the basic data we need for running a scanstatistics - counts, zones, baselines

score_cusum <- function(y, lambda, scaling) {
  # Applied to a purely temporal process
  drift <- lambda * (scaling - 1) / log(scaling)
  p <- 0
  for (ind in seq_along(y)) {
    p <- max(0, p + y[[ind]] - drift[[ind]])
  }
  return(p)
}

test_that("scan_cusum_poisson runs", {
  expect_error(scan_cusum_poisson(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                   n_mcsim = 10),
               NA)
})

test_that("scan_cusum_poisson gives the scores we expect", {
  scanres <- scan_cusum_poisson(wide_cases_sm, key_matrix_sm, wide_baseline_sm,
                                scaling = 1.5,
                                 n_mcsim = 1)
  calc_scores <- scanres$observed %>%
    dplyr::select(zone, duration, score) %>%
    dplyr::arrange(zone, duration) %>%
    tidyr::pivot_wider(names_from = zone, values_from = score) %>%
    dplyr::select(-duration) %>%
    as.matrix()

  expected_scores <- mapply(score_cusum, wide_cases_sm[2, ], wide_baseline_sm[2, ], 1.5)
  expect_equal(unname(unlist(calc_scores[1, ])),
               as.vector(expected_scores))
})

test_that("scan_cusum_poisson finds the logical mlc", {
  scanres <- scan_cusum_poisson(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                scaling = 1.5,
                                 n_mcsim = 1)
  y <- as.vector(wide_cases_lg[outbreak_tm_lg, outbreak_sp_lg])
  mu <- as.vector(wide_baseline_lg[outbreak_tm_lg, outbreak_sp_lg])
  expected_score <- score_cusum(y, mu, 1.5)

  expect_equal(scanres$MLC$score, expected_score)
  expect_equal(sort(scanres$MLC$locations),
               sort(outbreak_sp_lg))
  expect_equal(scanres$MLC$duration,
               length(outbreak_tm_lg))
})

test_that("scan_cusum_poisson returns sorted scores", {
  scanres <- scan_cusum_poisson(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                 n_mcsim = 1)
  expect_equal(scanres$observed$score, sort(scanres$observed$score, decreasing = TRUE))
})

test_that("We can call scan_cusum_poisson with zones and it'll calculate key_matrix", {
  expect_error(scan_cusum_poisson(wide_cases_lg, zones_lg, wide_baseline_lg,
                                   n_mcsim = 1),
               NA)
})

test_that("Can call scan_cusum_poisson with n_mcsim = 0", {
  expect_error(scan_cusum_poisson(wide_cases_sm, key_matrix_sm, wide_baseline_sm,
                                   n_mcsim = 0),
               NA)
})

test_that("replicates have reasonable sd", {
  scanres <- scan_cusum_poisson(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                n_mcsim = 10)
  expect_true(sd(scanres$replicates$score) > 1)
})
