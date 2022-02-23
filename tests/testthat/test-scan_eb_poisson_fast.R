library(here)
# source(here("tests", "testthat", "setup-alarm_function_data.R"))

score_ebp <- function(y, mu) {
  y <- sum(y)
  mu <- sum(mu)
  l <- y * log(y / mu) + (mu - y) #From Neill (2009)
  return(l)
}

test_that("scan_eb_poisson_fast runs", {
  expect_error(scan_eb_poisson_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                   n_mcsim = 10),
               NA)
})

test_that("scan_eb_poisson_fast gives the scores we expect", {
  scanres <- scan_eb_poisson_fast(wide_cases_sm, key_matrix_sm, wide_baseline_sm,
                                 n_mcsim = 1)
  calc_scores <- pivot_scan_results(scanres$observed, score)
  expected_scores <- mapply(score_ebp, wide_cases_sm[2, ], wide_baseline_sm[2, ])
  expect_equal(unname(unlist(calc_scores[1, ])),
               as.vector(expected_scores))
})

test_that("scan_eb_poisson_fast finds the logical mlc", {
  scanres <- scan_eb_poisson_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                 n_mcsim = 1)
  y <- as.vector(wide_cases_lg[outbreak_tm_lg, outbreak_sp_lg])
  mu <- as.vector(wide_baseline_lg[outbreak_tm_lg, outbreak_sp_lg])
  expected_score <- score_ebp(y, mu)

  expect_equal(scanres$MLC$score, expected_score)
  expect_equal(sort(scanres$MLC$locations),
               sort(outbreak_sp_lg))
  expect_equal(scanres$MLC$duration,
               length(outbreak_tm_lg))
})

test_that("scan_eb_poisson_fast returns sorted scores", {
  scanres <- scan_eb_poisson_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                 n_mcsim = 1)
  expect_equal(scanres$observed$score, sort(scanres$observed$score, decreasing = TRUE))
})

test_that("We can call scan_eb_poisson_fast with zones and it'll calculate key_matrix", {
  expect_error(scan_eb_poisson_fast(wide_cases_lg, zones_lg, wide_baseline_lg,
                                   n_mcsim = 1),
               NA)
})

test_that("Can call scan_eb_poisson_fast with n_mcsim = 0", {
  expect_error(scan_eb_poisson_fast(wide_cases_sm, key_matrix_sm, wide_baseline_sm,
                                   n_mcsim = 0),
               NA)
})
