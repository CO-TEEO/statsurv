library(here)
# source(here("tests", "testthat", "setup-alarm_function_data.R"))


check_overlap <- function(scanres, zones) {
  if (scanres$type == "Bayesian") {
    clusters <- scanres$posteriors$window_posteriors
  } else {
    clusters <- scanres$observed
  }
  expect_true(anyDuplicated(unlist(zones[clusters$zone])) == 0)
  expect_equal(sort(unique(unlist(zones[clusters$zone]))), sort(unique(unlist(zones))))
}

check_overlap_frac <- function(scanres, zones, max_overlap_frac) {
  if (scanres$type == "Bayesian") {
    clusters <- scanres$posteriors$window_posteriors
  } else {
    clusters <- scanres$observed
  }
  obs_z <- clusters$zone
  prev_seen_loc <- c()
  for (z in obs_z) {
    locs <- zones[[z]]
    overlap_frac <- sum(locs %in% prev_seen_loc) / length(locs)
    expect_true(overlap_frac <= max_overlap_frac)
    prev_seen_loc <- unique(c(prev_seen_loc, locs))
  }
}

check_all <- function(scanres, zones) {
  # Check order by score, order by relrisk, and take_highest = TRUE, take_highest = FALSE
  filt_res1 <- remove_overlapping_clusters(scanres, zones,
                                           order_by = score,
                                           take_highest = TRUE,
                                           max_overlap_frac = 0)

  check_overlap(filt_res1, zones)
  expect_equal(dplyr::arrange(scanres$observed, dplyr::desc(score))[1, ],
               filt_res1$observed[1, ])

  filt_res2 <- remove_overlapping_clusters(scanres, zones,
                                           order_by = score,
                                           take_highest = FALSE,
                                           max_overlap_frac = 0)
  check_overlap(filt_res2, zones)
  expect_equal(dplyr::arrange(scanres$observed, score)[1, ],
               filt_res2$observed[1, ])

  filt_res3 <- remove_overlapping_clusters(scanres, zones,
                                           order_by = duration,
                                           take_highest = TRUE,
                                           max_overlap_frac = 0)

  check_overlap(filt_res3, zones)
  expect_equal(dplyr::arrange(scanres$observed, dplyr::desc(duration))[1, ],
               filt_res3$observed[1, ])
  filt_res4 <- remove_overlapping_clusters(scanres, zones,
                                           order_by = score,
                                           take_highest = TRUE,
                                           max_overlap_frac = 1)
  check_overlap_frac(filt_res4, zones, 1)
  filt_res5 <- remove_overlapping_clusters(scanres, zones,
                                           order_by = score,
                                           take_highest = TRUE,
                                           max_overlap_frac = 0.5)
  check_overlap_frac(filt_res5, zones, 0.5)
  invisible(1)
}

test_that("Works on scan_eb_poisson", {
  scanres <- scan_eb_poisson(wide_cases_lg, zones_lg, wide_baseline_lg)
  check_all(scanres, zones_lg)

  scanres2 <- scan_eb_poisson2(spacetime_data_lg, cases, zones_lg, .fitted)
  check_all(scanres2, zones_lg)
})

test_that("Works on scan_eb_poisson_fast", {
  scanres <- scan_eb_poisson_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg, n_mcsim = 10)
  check_all(scanres, zones_lg)

  scanres2 <- scan_eb_poisson_fast2(spacetime_data_lg, cases, zones_lg, .fitted)
  check_all(scanres2, zones_lg)
})

test_that("Works on scan_pb_poisson", {
  scanres <- scan_pb_poisson(wide_cases_lg, zones_lg, population = wide_pop_lg)
  check_all(scanres, zones_lg)

  scanres2 <- scan_pb_poisson2(spacetime_data_lg, cases, zones_lg, pop)
  check_all(scanres2, zones_lg)
})

test_that("Works on scan_eb_zip", {
  scanres <- scan_eb_zip(wide_cases_lg, zones_lg, baselines = wide_baseline_lg,
                         probs = wide_probs_lg)
  check_all(scanres, zones_lg)

  scanres2 <- scan_eb_zip2(spacetime_data_lg, cases, zones_lg, .fitted, thetas)
  check_all(scanres2, zones_lg)
})

test_that("Works on scan_permutation", {
  scanres <- scan_permutation(wide_cases_lg, zones_lg, population = wide_pop_lg)
  check_all(scanres, zones_lg)

  scanres2 <- scan_permutation2(spacetime_data_lg, cases, zones_lg, pop)
  check_all(scanres2, zones_lg)
})

test_that("Works on scan_eb_negbin", {
  scanres <- scan_eb_negbin(wide_cases_lg, zones_lg, baseline = wide_baseline_lg,
                            thetas = wide_thetas_lg)
  check_all(scanres, zones_lg)

  scanres2 <- scan_eb_negbin2(spacetime_data_lg, cases, zones_lg, .fitted, thetas)
  check_all(scanres2, zones_lg)
})

test_that("Works on scan_eb_negbin_fast", {
  scanres <- scan_eb_negbin_fast(wide_cases_lg, key_matrix_lg, wide_baseline = wide_baseline_lg,
                            thetas = wide_thetas_lg)
  check_all(scanres, zones_lg)

  scanres2 <- scan_eb_negbin_fast2(spacetime_data_lg, cases, zones_lg, .fitted, thetas)
  check_all(scanres2, zones_lg)
})

test_that("Works on scan_bayes_negbin", {
  scanres <- scan_bayes_negbin(wide_cases_lg, zones_lg, baseline = wide_baseline_lg)

  filt_res1 <- remove_overlapping_clusters(scanres, zones_lg,
                                           order_by = log_posterior ,
                                           take_highest = TRUE,
                                           max_overlap_frac = 0)

  check_overlap(filt_res1, zones_lg)
  expect_equal(dplyr::arrange(scanres$posteriors$window_posteriors, dplyr::desc(log_posterior))[1, ],
               filt_res1$posteriors$window_posteriors[1, ])

  filt_res2 <- remove_overlapping_clusters(scanres, zones_lg,
                                           order_by = log_posterior,
                                           take_highest = FALSE,
                                           max_overlap_frac = 0)
  check_overlap(filt_res2, zones_lg)
  expect_equal(dplyr::arrange(scanres$posteriors$window_posteriors, log_posterior)[1, ],
               filt_res2$posteriors$window_posteriors[1, ])

  filt_res3 <- remove_overlapping_clusters(scanres, zones_lg,
                                           order_by = duration,
                                           take_highest = TRUE,
                                           max_overlap_frac = 0)

  check_overlap(filt_res3, zones_lg)
  expect_equal(dplyr::arrange(scanres$posteriors$window_posteriors, dplyr::desc(duration))[1, ],
               filt_res3$posteriors$window_posteriors[1, ])
  filt_res4 <- remove_overlapping_clusters(scanres, zones_lg,
                                           order_by = log_posterior,
                                           take_highest = TRUE,
                                           max_overlap_frac = 1)
  check_overlap_frac(filt_res4, zones_lg, 1)
  filt_res5 <- remove_overlapping_clusters(scanres, zones_lg,
                                           order_by = log_posterior,
                                           take_highest = TRUE,
                                           max_overlap_frac = 0.5)
  check_overlap_frac(filt_res5, zones_lg, 0.5)
})

test_that("Works on scan_bayes_negbin2", {
  scanres2 <- scan_bayes_negbin2(spacetime_data_lg, cases, zones_lg, .fitted)

  filt_res1 <- remove_overlapping_clusters(scanres2, zones_lg,
                                           order_by = log_posterior,
                                           take_highest = TRUE,
                                           max_overlap_frac = 0)

  check_overlap(filt_res1, zones_lg)
  expect_equal(dplyr::arrange(scanres2$posteriors$window_posteriors, dplyr::desc(log_posterior))[1, ],
               filt_res1$posteriors$window_posteriors[1, ])

  filt_res2 <- remove_overlapping_clusters(scanres2, zones_lg,
                                           order_by = log_posterior,
                                           take_highest = FALSE,
                                           max_overlap_frac = 0)
  check_overlap(filt_res2, zones_lg)
  expect_equal(dplyr::arrange(scanres2$posteriors$window_posteriors, log_posterior)[1, ],
               filt_res2$posteriors$window_posteriors[1, ])

  filt_res3 <- remove_overlapping_clusters(scanres2, zones_lg,
                                           order_by = duration,
                                           take_highest = TRUE,
                                           max_overlap_frac = 0)

  check_overlap(filt_res3, zones_lg)
  expect_equal(dplyr::arrange(scanres2$posteriors$window_posteriors, dplyr::desc(duration))[1, ],
               filt_res3$posteriors$window_posteriors[1, ])
  filt_res4 <- remove_overlapping_clusters(scanres2, zones_lg,
                                           order_by = log_posterior,
                                           take_highest = TRUE,
                                           max_overlap_frac = 1)
  check_overlap_frac(filt_res4, zones_lg, 1)
  filt_res5 <- remove_overlapping_clusters(scanres2, zones_lg,
                                           order_by = log_posterior,
                                           take_highest = TRUE,
                                           max_overlap_frac = 0.5)
  check_overlap_frac(filt_res5, zones_lg, 0.5)
})
