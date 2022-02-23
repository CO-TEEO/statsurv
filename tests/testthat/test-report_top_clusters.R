# What do I want to test here?
# 1. Works with multiple different alarm statistics (bayes, eb_poisson, eb_poisson_fast)
# -2. We can specify different columns to order by.
# -3. max_reported behaves correctly - we never get more than max allowed results
# -4. min_reported behaves correctly - we always get at leas this number of reported clusters.
# 5. single_duration behaves correctly - if TRUE, we only see each zone at most once.

# So, what are some concrete tests:
# If single_duration = TRUE, we see every zone
qmin <- function(v) {
  if (length(v) == 0) {
    Inf
  } else {
    min(v)
  }
}

qmax <- function(v) {
  if (length(v) == 0) {
    -Inf
  } else {
    max(v)
  }
}

check_single_duration <- function(scanres, col) {
  th <- c(TRUE, FALSE)
  cv <- c(-40, 0, 2, 4, 40, NULL)
  mxr = c(2, 5, 10, 100)

  for (i in 1:20) {
    curr_max <- sample(mxr, 1)
    curr_min <- sample(curr_max, 1)
    res <- report_top_clusters(scanres, {{col}},
                               take_highest = sample(th, 1),
                               cutoff_val = sample(cv, 1),
                               max_reported = curr_max,
                               min_reported = curr_min,
                               single_duration = TRUE)
    expect_false(any(duplicated(res$zone)))
  }

  any_dup <- FALSE
  for (i in 1:50) {
    curr_max <- sample(mxr, 1)
    curr_min <- sample(curr_max, 1)
    res <- report_top_clusters(scanres, {{col}},
                               take_highest = sample(th, 1),
                               cutoff_val = sample(cv, 1),
                               max_reported = curr_max,
                               min_reported = curr_min,
                               single_duration = FALSE)
    if (any(duplicated(res$zone))) {
      any_dup <- TRUE
      break
    }
  }
  expect_true(any_dup)
}

check_minmax <- function(scanres, col) {
  wanted_vals <- c(-40, 0, 2, 4, 40, NULL)
  for (v in wanted_vals) {
    res <- report_top_clusters(scanres, {{col}}, take_highest = TRUE, cutoff_val = v,
                               min_reported = 5)
    expect_true(nrow(res) >= 5)

    res <- report_top_clusters(scanres, {{col}}, take_highest = FALSE, cutoff_val = v,
                               min_reported = 5)
    expect_true(nrow(res) >= 5)
  }

  for (v in wanted_vals) {
    res <- report_top_clusters(scanres, {{col}}, take_highest = TRUE, cutoff_val = v,
                               max_reported = 5)
    expect_true(nrow(res) <= 5)

    res <- report_top_clusters(scanres, {{col}}, take_highest = FALSE, cutoff_val = v,
                               max_reported = 5)
    expect_true(nrow(res) <= 5)
  }

  for (v in wanted_vals) {
    res <- report_top_clusters(scanres, {{col}}, take_highest = TRUE, cutoff_val = v,
                               max_reported = 5, min_reported = 5)
    expect_true(nrow(res) == 5)

    res <- report_top_clusters(scanres, {{col}}, take_highest = FALSE, cutoff_val = v,
                               max_reported = 5, min_reported = 5)
    expect_true(nrow(res) == 5)
  }


}

check_cutoff <- function(scanres, col) {
  cutoff_val <- runif(1, 0, 30)
  res <- report_top_clusters(scanres, {{col}}, take_highest = TRUE, min_reported = 0,
                             cutoff_val = cutoff_val)
  expect_true(qmin(dplyr::pull(res, {{col}})) >= cutoff_val)

  cutoff_val <- runif(1, 0, 30)
  res <- report_top_clusters(scanres, {{col}}, take_highest = FALSE, min_reported = 0,
                             cutoff_val = cutoff_val)
  expect_true(qmax(dplyr::pull(res, {{col}})) <= cutoff_val)
}

check_all <- function(scanres, col, col2) {
  check_cutoff(scanres, {{col}})
  check_minmax(scanres, {{col}})
  check_single_duration(scanres, {{col}})

  check_cutoff(scanres, {{col2}})
  check_minmax(scanres, {{col2}})
  check_single_duration(scanres, {{col2}})
}

library(here)
library(scanstatistics)
# source(here("tests", "testthat", "setup-alarm_function_data.R"))

test_that("Works on scan_eb_poisson", {
  scanres <- scan_eb_poisson(wide_cases_lg, zones_lg, wide_baseline_lg)
  check_all(scanres, score, duration)

  scanres2 <- scan_eb_poisson2(spacetime_data_lg, cases, zones_lg, .fitted)
  check_all(scanres2, score, duration)
})

test_that("Works on scan_eb_poisson_fast", {
  scanres <- scan_eb_poisson_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg, n_mcsim = 10)
  check_all(scanres, score, duration)

  scanres2 <- scan_eb_poisson_fast2(spacetime_data_lg, cases, zones_lg, .fitted)
  check_all(scanres2, score, duration)
})

test_that("Works on scan_pb_poisson", {
  scanres <- scan_pb_poisson(wide_cases_lg, zones_lg, population = wide_pop_lg)
  check_all(scanres, score, duration)

  scanres2 <- scan_pb_poisson2(spacetime_data_lg, cases, zones_lg, pop)
  check_all(scanres2, score, duration)
})

test_that("Works on scan_eb_zip", {
  scanres <- scan_eb_zip(wide_cases_lg, zones_lg, baselines = wide_baseline_lg,
                         probs = wide_probs_lg)
  check_all(scanres, score, duration)

  scanres2 <- scan_eb_zip2(spacetime_data_lg, cases, zones_lg, .fitted, thetas)
  check_all(scanres2, score, duration)
  check_all(scanres2, action_level, duration)
})

test_that("Works on scan_permutation", {
  scanres <- scan_permutation(wide_cases_lg, zones_lg, population = wide_pop_lg)
  check_all(scanres, score, duration)

  scanres2 <- scan_permutation2(spacetime_data_lg, cases, zones_lg, pop)
  check_all(scanres2, score, duration)
})

test_that("Works on scan_eb_negbin", {
  scanres <- scan_eb_negbin(wide_cases_lg, zones_lg, baseline = wide_baseline_lg,
                            thetas = wide_thetas_lg)
  check_all(scanres, score, duration)

  scanres2 <- scan_eb_negbin2(spacetime_data_lg, cases, zones_lg, .fitted, thetas)
  check_all(scanres2, score, duration)
})

test_that("Works on scan_eb_negbin_fast", {
  scanres <- scan_eb_negbin_fast(wide_cases_lg, key_matrix_lg, wide_baseline = wide_baseline_lg,
                                 thetas = wide_thetas_lg)
  check_all(scanres, score, duration)

  scanres2 <- scan_eb_negbin_fast2(spacetime_data_lg, cases, zones_lg, .fitted, thetas)
  check_all(scanres2, action_level, duration)
})

test_that("Works on scan_bayes_negbin", {
  scanres <- scan_bayes_negbin(wide_cases_lg, zones_lg, baseline = wide_baseline_lg)
  check_all(scanres, log_posterior, duration)

  scanres2 <- scan_bayes_negbin2(spacetime_data_lg, cases, zones_lg, .fitted)
  check_all(scanres2, action_level, duration)
})
