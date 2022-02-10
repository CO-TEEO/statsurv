library(here)
source(here("tests", "testthat", "build_data_for_alarm_functions.R"))


score_shewhart <- function(y, mean, sigma) {
  return((y - mean )/ sigma)
}

test_that("parallel_shewhart_gaussian runs", {
  expect_error(parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg),
               NA)
})

test_that("parallel_shewhart_gaussian gives the scores we expect", {
  scanres <- parallel_shewhart_gaussian(wide_cases_sm, wide_baseline_sm, mean = 0, sigma = 1)

  scores1 <- score_shewhart(wide_cases_sm[, 1] - wide_baseline_sm[, 1],
                         mean = 0, sigma = 1)
  scores2 <- score_shewhart(wide_cases_sm[, 2] - wide_baseline_sm[, 2],
                            mean = 0, sigma = 1)

  expect_equal(scanres[, 1],
               scores1)
  expect_equal(scanres[, 2],
               scores2)
})

test_that("Constant baseline and mean are equivalent", {
  scanres1 <- parallel_shewhart_gaussian(wide_cases_sm, mean = 2, sigma = 1)
  scanres2 <- parallel_shewhart_gaussian(wide_cases_sm,
                                         wide_baseline_sm,
                                         mean = 0, sigma = 1)
  expect_equal(scanres1, scanres2)
})

test_that("We can provide mean and sigma as scalars, vectors, or matrices", {
  scanres_ref <- parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg,
                                        mean = 0.1, sigma = 1.2)

  v_mean <- c(rep(0.1, 8), 3) # Special scaling for region 9
  scanres <- parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg,
                                    mean = v_mean, sigma = 1.2)
  diff_c <- 9
  expect_equal(scanres_ref[, -diff_c], scanres[, -diff_c])
  v_sigma = c(rep(1.2, 8), 3)
  scanres <- parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg,
                                        mean = 0.1, sigma = v_sigma)
  expect_equal(scanres_ref[, -diff_c], scanres[, -diff_c])


  diff_i <- 1
  m_mean <- matrix(0.1, nrow = 4, ncol = 9)
  m_mean[4, 1] <- 3

  scanres <- parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg,
                                    mean = m_mean, sigma = 1.2)
  expect_equal(scanres_ref[, -diff_i], scanres[, -diff_i])

  m_sigma <- matrix(1.2, nrow = 4, ncol = 9)
  m_sigma[4, 1] <- 3
  scanres <- parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg,
                                        mean = 0.1, sigma = m_sigma)
  expect_equal(scanres_ref[, -diff_i], scanres[, -diff_i])
})


