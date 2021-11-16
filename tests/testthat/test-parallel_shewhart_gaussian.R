source("build_data_for_alarm_functions.R")


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


