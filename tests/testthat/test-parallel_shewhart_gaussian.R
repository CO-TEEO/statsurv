library(here)
# source(here("tests", "testthat", "setup-alarm_function_data.R"))

score_shewhart <- function(y, mean, sigma) {
  return((y - mean )/ sigma)
}

clean_up_v2 <- function(scanres2) {
  m2 <- pivot_for_scan(scanres2, .action_level)
  dimnames(m2) <- list(NULL, NULL)
  m2
}

test_that("parallel_shewhart_gaussian runs", {
  expect_error(parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg),
               NA)
  expect_error(parallel_shewhart_gaussian2(spacetime_data_lg, cases, .fitted),
               NA)
})

test_that("parallel_shewhart_gaussian gives the scores we expect", {
  scanres <- parallel_shewhart_gaussian(wide_cases_sm, wide_baseline_sm, mean = 0, sigma = 1)
  scanres2 <- parallel_shewhart_gaussian2(spacetime_data_sm, cases, .fitted, mean = 0, sigma = 1)

  scores1 <- score_shewhart(wide_cases_sm[, 1] - wide_baseline_sm[, 1],
                         mean = 0, sigma = 1)
  scores2 <- score_shewhart(wide_cases_sm[, 2] - wide_baseline_sm[, 2],
                            mean = 0, sigma = 1)

  expect_equal(scanres[, 1],
               scores1)
  expect_equal(scanres[, 2],
               scores2)

  expect_equal(dplyr::filter(scanres2, id_space == 1)$.action_level, scores1)
  expect_equal(dplyr::filter(scanres2, id_space == 2)$.action_level, scores2)
})

test_that("Constant baseline and mean are equivalent", {
  scanres_mean <- parallel_shewhart_gaussian(wide_cases_sm, mean = 2, sigma = 1)
  scanres_baseline <- parallel_shewhart_gaussian(wide_cases_sm,
                                         wide_baseline_sm,
                                         mean = 0, sigma = 1)
  expect_equal(scanres_mean, scanres_baseline)

  scanres2_mean <- parallel_shewhart_gaussian2(spacetime_data_sm, cases, mean = 2, sigma = 1)
  scanres2_baseline <-
    parallel_shewhart_gaussian2(spacetime_data_sm, cases, baseline_col = .fitted,
                                mean = 0, sigma = 1)
  expect_equal(scanres2_mean, scanres2_baseline)
  expect_equal(scanres_mean, clean_up_v2(scanres2_mean))
})

test_that("We can provide mean and sigma as scalars, vectors, or matrices", {
  scanres_ref <- parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg,
                                        mean = 0.1, sigma = 1.2)

  v_mean <- c(rep(0.1, 8), 3) # Special scaling for region 9

  scanres <- parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg,
                                    mean = v_mean, sigma = 1.2)
  diff_c <- 9
  expect_equal(scanres_ref[, -diff_c], scanres[, -diff_c])

  adj_data_lg <- spacetime_data_lg %>%
    dplyr::mutate(mean_col = ifelse(id_space == 9, 3, 0.1))
  scanres2 <- parallel_shewhart_gaussian2(adj_data_lg, cases, .fitted,
                                          mean = mean_col, sigma = 1.2)
  expect_equal(scanres, clean_up_v2(scanres2))

  v_sigma = c(rep(1.2, 8), 3)
  scanres <- parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg,
                                        mean = 0.1, sigma = v_sigma)
  expect_equal(scanres_ref[, -diff_c], scanres[, -diff_c])

  adj_data_lg <- spacetime_data_lg %>%
    dplyr::mutate(sigma_col = ifelse(id_space == 9, 3, 1.2))
  scanres2 <- parallel_shewhart_gaussian2(adj_data_lg, cases, .fitted,
                                          mean = 0.1, sigma = sigma_col)
  expect_equal(scanres, clean_up_v2(scanres2))


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

  adj_data_lg <- spacetime_data_lg %>%
    dplyr::mutate(sigma_col = ifelse(id_space == 1 & id_time == 4, 3, 1.2))
  scanres2 <- parallel_shewhart_gaussian2(adj_data_lg, cases, .fitted,
                                          mean = 0.1, sigma = sigma_col)
  expect_equal(scanres, clean_up_v2(scanres2))
})


