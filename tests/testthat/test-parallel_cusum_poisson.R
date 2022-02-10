source("build_data_for_alarm_functions.R")
# Gets us the basic data we need for running a scanstatistics - counts, zones, baselines

score_cusum <- function(y, lambda, scaling) {
  # Applied to a purely temporal process
  drift <- lambda * (scaling - 1) / log(scaling)
  p <- 0
  all_p <- rep(NA, length(y))
  for (ind in seq_along(y)) {
    p <- max(0, p + y[[ind]] - drift[[ind]])
    all_p[[ind]] <- p
  }
  return(all_p)
}

clean_up_v2 <- function(scanres2) {
  m2 <- pivot_for_scan(scanres2, .action_level)
  dimnames(m2) <- list(NULL, NULL)
  m2
}
test_that("parallel_cusum_poisson runs", {
  expect_error(parallel_cusum_poisson(wide_cases_lg, wide_baseline_lg),
               NA)
})

test_that("parallel_cusum_poisson gives the scores we expect", {
  scanres <- parallel_cusum_poisson(wide_cases_sm, wide_baseline_sm, scaling = 1.5)
  scanres2 <- parallel_cusum_poisson2(spacetime_data_sm, cases, .fitted, scaling = 1.5)

  scores1 <- score_cusum(wide_cases_sm[, 1],
                         wide_baseline_sm[, 1],
                         scaling = 1.5)
  scores2 <- score_cusum(wide_cases_sm[, 2],
                         wide_baseline_sm[, 2],
                         scaling = 1.5)

  expect_equal(scanres[, 1],
               scores1)
  expect_equal(scanres[, 2],
               scores2)
  expect_equal(dplyr::filter(scanres2, id_space == 1)$.action_level, scores1)
  expect_equal(dplyr::filter(scanres2, id_space == 2)$.action_level, scores2)
})

test_that("parallel_cusum_poisson scores are higher if cases are higher", {
  scanres <- parallel_cusum_poisson(wide_cases_lg, wide_baseline_lg,
                                scaling = 1.5)
  scanres2 <- parallel_cusum_poisson2(spacetime_data_lg, cases, .fitted, scaling = 1.5)

  expect_equal(order(scanres[4, ]), order(wide_cases_lg[4, ]))
  expect_equal(order(dplyr::filter(scanres2, id_time == 4)$.action_level),order(wide_cases_lg[4, ]))
})

test_that("parallel_cusum_poisson returns a matrix the same dimensions as wide_cases", {
  scanres <- parallel_cusum_poisson(wide_cases_lg, wide_baseline_lg)
  expect_equivalent(scanres * 0, wide_cases_lg * 0)
})

test_that("We scan specify scaling in 3 different ways", {
  scanres_ref <- parallel_cusum_poisson(wide_cases_lg, wide_baseline_lg,
                                    scaling = 1.5)
  scanres2_ref <- parallel_cusum_poisson2(spacetime_data_lg, cases, .fitted,
                                          scaling = 1.5)

  v_scaling <- c(rep(1.5, 8), 3) # Special scaling for region 9
  scanres <- parallel_cusum_poisson(wide_cases_lg, wide_baseline_lg,
                                    scaling = v_scaling)
  diff_c <- 9
  expect_equal(scanres_ref[, -diff_c], scanres[, -diff_c])

  diff_i <- 4
  m_scaling <- matrix(1.5, nrow = 4, ncol = 9)
  m_scaling[4, 1] <- 3
  adj_data_lg <- spacetime_data_lg %>%
    dplyr::mutate(scaling = ifelse(id_time == 4 & id_space == 1, 3, 1.5))

  scanres <- parallel_cusum_poisson(wide_cases_lg, wide_baseline_lg,
                                    scaling = m_scaling)
  scanres2 <- parallel_cusum_poisson2(adj_data_lg, cases, .fitted, scaling = scaling)
  expect_equal(scanres_ref[-diff_i], scanres[-diff_i])
  expect_equal(scanres, clean_up_v2(scanres2))
})

