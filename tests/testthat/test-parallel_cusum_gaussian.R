library(here)
# source(here("tests", "testthat", "setup-alarm_function_data.R"))

score_cusum <- function(y, baseline, mean = 0, sigma = 1, drift = 0.5) {
  # Applied to a purely temporal process

  p <- 0
  all_p <- rep(NA, length(y))
  for (ind in seq_along(y)) {
    p <- max(0, p + (y[[ind]] - baseline[[ind]] - mean)/sigma - drift)
    all_p[[ind]] <- p
  }
  return(all_p)
}

clean_up_v2 <- function(scanres2) {
  m2 <- pivot_for_scan(scanres2, .action_level)
  dimnames(m2) <- list(NULL, NULL)
  m2
}
test_that("parallel_cusum_gaussian runs", {
  expect_error(parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg),
               NA)
  expect_error(parallel_cusum_gaussian2(spacetime_data_lg, cases, .fitted),
               NA)
})

test_that("parallel_cusum_gaussian gives the scores we expect", {
  scanres <- parallel_cusum_gaussian(wide_cases_sm, wide_baseline_sm, drift = 0.75)
  scanres2 <- parallel_cusum_gaussian2(spacetime_data_sm, cases, .fitted, drift = 0.75)

  scores1 <- score_cusum(wide_cases_sm[, 1],
                         wide_baseline_sm[, 1],
                         drift = 0.75)
  scores2 <- score_cusum(wide_cases_sm[, 2],
                         wide_baseline_sm[, 2],
                         drift = 0.75)

  expect_equal(scanres[, 1],
               scores1)
  expect_equal(scanres[, 2],
               scores2)
  expect_equal(dplyr::filter(scanres2, id_space == 1)$.action_level, scores1)
  expect_equal(dplyr::filter(scanres2, id_space == 2)$.action_level, scores2)
})

test_that("parallel_cusum_gaussian scores are higher if cases are higher", {
  scanres <- parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg,
                                     drift = 0.75)
  scanres2 <- parallel_cusum_gaussian2(spacetime_data_lg, cases, .fitted, drift = 0.75)

  expect_equal(order(scanres[4, ]), order(wide_cases_lg[4, ]))
  expect_equal(order(dplyr::filter(scanres2, id_time == 4)$.action_level),order(wide_cases_lg[4, ]))
})

test_that("parallel_cusum_gaussian returns a matrix the same dimensions as wide_cases", {
  scanres <- parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg)
  expect_equivalent(scanres * 0, wide_cases_lg * 0)
})


test_that("We can provide mean, sigma, and drift as scalars, vectors, or matrices", {
  scanres_ref <- parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg,
                                         mean = 0.1, sigma = 1.2, drift = 0.75)

  v_mean <- c(rep(0.1, 8), 3) # Special scaling for region 9

  scanres <- parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg,
                                        mean = v_mean, sigma = 1.2, drift = 0.75)
  diff_c <- 9
  expect_equal(scanres_ref[, -diff_c], scanres[, -diff_c])

  adj_data_lg <- spacetime_data_lg %>%
    dplyr::mutate(mean_col = ifelse(id_space == 9, 3, 0.1))
  scanres2 <- parallel_cusum_gaussian2(adj_data_lg, cases, .fitted,
                                       mean = mean_col, sigma = 1.2, drift = 0.75)
  expect_equal(scanres, clean_up_v2(scanres2))

  v_sigma = c(rep(1.2, 8), 3)
  scanres <- parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg,
                                        mean = 0.1, sigma = v_sigma, drift = 0.75)
  expect_equal(scanres_ref[, -diff_c], scanres[, -diff_c])

  adj_data_lg <- spacetime_data_lg %>%
    dplyr::mutate(sigma_col = ifelse(id_space == 9, 3, 1.2))
  scanres2 <- parallel_cusum_gaussian2(adj_data_lg, cases, .fitted,
                                      mean = 0.1, sigma = sigma_col, drift = 0.75)
  expect_equal(scanres, clean_up_v2(scanres2))

  v_drift <- c(rep(0.75, 8), 1.2)
  scanres <- parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg,
                                     mean = 0.1, sigma = 1.2, drift = v_drift)
  expect_equal(scanres_ref[, -diff_c], scanres[, -diff_c])

  adj_data_lg <- spacetime_data_lg %>%
    dplyr::mutate(drift_col = ifelse(id_space == 9, 1.2, 0.75))
  scanres2 <- parallel_cusum_gaussian2(adj_data_lg, cases, .fitted,
                                       mean = 0.1, sigma = 1.2, drift = drift_col)
  expect_equal(scanres, clean_up_v2(scanres2))


  diff_i <- 1
  m_mean <- matrix(0.1, nrow = 4, ncol = 9)
  m_mean[4, 1] <- 3

  scanres <- parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg,
                                     mean = m_mean, sigma = 1.2, drift = 0.75)
  expect_equal(scanres_ref[, -diff_i], scanres[, -diff_i])

  m_sigma <- matrix(1.2, nrow = 4, ncol = 9)
  m_sigma[4, 1] <- 3
  scanres <- parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg,
                                     mean = 0.1, sigma = m_sigma, drift = 0.75)
  expect_equal(scanres_ref[, -diff_i], scanres[, -diff_i])

  adj_data_lg <- spacetime_data_lg %>%
    dplyr::mutate(sigma_col = ifelse(id_space == 1 & id_time == 4, 3, 1.2))
  scanres2 <- parallel_cusum_gaussian2(adj_data_lg, cases, .fitted,
                                       mean = 0.1, sigma = sigma_col, drift = 0.75)
  expect_equal(scanres, clean_up_v2(scanres2))


  m_drift <- matrix(0.75, nrow = 4, ncol = 9)
  m_drift[4, 1] <- 3
  scanres <- parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg,
                                     mean = 0.1, sigma = 1.2, drift = m_drift)
  expect_equal(scanres_ref[, -diff_i], scanres[, -diff_i])



})


