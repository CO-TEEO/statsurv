source("build_data_for_alarm_functions.R")
# Gets us the basic data we need for running a scanstatistics - counts, zones, baselines
score_ebnb <- function(y, mu, theta) {
  omega <- 1 + mu / theta
  numer <- sum(y / omega - mu / omega)
  denom <- sqrt(sum(mu / omega))
  return(numer / denom)
}

test_that("scan_eb_negbin_fast runs", {
  expect_error(scan_eb_negbin_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                   n_mcsim = 10),
               NA)
})

test_that("scan_eb_negbin_fast gives the scores we expect", {
  scanres <- scan_eb_negbin_fast(wide_cases_sm, key_matrix_sm, wide_baseline_sm,
                                 n_mcsim = 1)
  calc_scores <- scanres$observed %>%
    pivot_for_scan(value_col = "score", row_coord = "duration", column_coord = "zone")
  expected_scores <- mapply(score_ebnb, wide_cases_sm[2, ], wide_baseline_sm[2, ], 1)
  expect_equal(flatten(calc_scores[1, ]),
               as.vector(expected_scores))
})

test_that("scan_eb_negbin_fast finds the logical mlc", {
  scanres <- scan_eb_negbin_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                 n_mcsim = 1)
  y <- as.vector(wide_cases_lg[outbreak_tm_lg, outbreak_sp_lg])
  mu <- as.vector(wide_baseline_lg[outbreak_tm_lg, outbreak_sp_lg])
  expected_score <- score_ebnb(y, mu, 1)

  expect_equal(scanres$MLC$score, expected_score)
  expect_equal(sort(scanres$MLC$locations),
               sort(outbreak_sp_lg))
  expect_equal(scanres$MLC$duration,
               length(outbreak_tm_lg))
})

test_that("scan_eb_negbin_fast returns sorted scores", {
  scanres <- scan_eb_negbin_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                 n_mcsim = 1)
  expect_equal(scanres$observed$score, sort(scanres$observed$score, decreasing = TRUE))
})

test_that("We can call scan_eb_negbin_fast with zones and it'll calculate key_matrix", {
  expect_error(scan_eb_negbin_fast(wide_cases_lg, zones_lg, wide_baseline_lg,
                                   n_mcsim = 1),
               NA)
})

test_that("Can call scan_eb_negbin_fast with n_mcsim = 0", {
  expect_error(scan_eb_negbin_fast(wide_cases_sm, key_matrix_sm, wide_baseline_sm,
                                   n_mcsim = 0),
               NA)
})
