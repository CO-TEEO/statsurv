set.seed(373953261)
library("scanstatistics")
library("here")
source(here("tests", "testthat", "build_data_for_alarm_functions.R"))
n_sim <- 0

destandardize <- function(alarm_res) {
  if (attr(alarm_res, "alarm_type") == "scan") {
    if (alarm_res$type == "Bayesian") {
      alarm_res$observed <- NULL
      alarm_res$MLC$action_level <- NULL
    } else {
      alarm_res$observed$action_level <- NULL
      alarm_res$replicates$action_level <- NULL
      alarm_res$MLC$action_level <- NULL
    }
    alarm_res$zone_info <- NULL
  }
  attr(alarm_res, "alarm_type") <- NULL
  return(alarm_res)
}

test_that("scan_eb_poisson matches", {
  scanres <- scan_eb_poisson(wide_cases_lg, zones_lg, baseline = wide_baseline_lg,
                             n_mcsim = n_sim)

  scanres2 <- scan_eb_poisson2(spacetime_data_lg, cases, zones_lg, .fitted,
                               n_mcsim = n_sim)

  expect_equal(destandardize(scanres2), scanres)

  scanres <- scan_eb_poisson(wide_cases_lg, zones_lg, n_mcsim = n_sim)
  scanres2 <- scan_eb_poisson2(spacetime_data_lg, cases, zones_lg, n_mcsim = n_sim)
  expect_equal(destandardize(scanres2), scanres)

  scanres <- scan_eb_poisson(wide_cases_lg, zones_lg,
                             population = wide_pop_lg,
                             n_mcsim = n_sim)
  scanres2 <- scanres2 <- scan_eb_poisson2(spacetime_data_lg, cases, zones_lg,
                                           pop_col = pop,
                                           n_mcsim = n_sim)
  expect_equal(destandardize(scanres2), scanres)
})

test_that("scan_pb_poisson matches", {
  scanres <- scan_pb_poisson(wide_cases_lg, zones_lg, population = wide_pop_lg,
                             n_mcsim = n_sim,
                             gumbel = TRUE)
  scanres2 <- scan_pb_poisson2(spacetime_data_lg, cases, zones_lg, pop_col = pop,
                               n_mcsim = n_sim,
                               gumbel = TRUE)
  expect_equal(destandardize(scanres2), scanres)

  scanres <- scan_pb_poisson(wide_cases_lg, zones_lg,
                             n_mcsim = n_sim,
                             gumbel = TRUE)
  scanres2 <- scan_pb_poisson2(spacetime_data_lg, cases, zones_lg,
                               n_mcsim = n_sim,
                               gumbel = TRUE)
  expect_equal(destandardize(scanres2), scanres)
  scanres2 <- scan_pb_poisson2(spacetime_data_lg, cases, zones_lg, pop_col = NULL,
                               n_mcsim = n_sim,
                               gumbel = TRUE)
  expect_equal(destandardize(scanres2), scanres)
})

test_that("scan_eb_negbin matches", {
  scanres <- scan_eb_negbin(wide_cases_lg, zones_lg, baseline = wide_baseline_lg,
                             n_mcsim = n_sim,
                             thetas = 1.5)
  scanres2 <- scan_eb_negbin2(spacetime_data_lg, cases, zones_lg, baseline_col = .fitted,
                              n_mcsim = n_sim,
                              theta_col = 1.5)
  expect_equal(destandardize(scanres2), scanres)

  scanres <- scan_eb_negbin(wide_cases_lg, zones_lg, baseline = wide_baseline_lg,
                            n_mcsim = n_sim, type = "emerging",
                            thetas = 1.5)
  scanres2 <- scan_eb_negbin2(spacetime_data_lg, cases, zones_lg, baseline_col = .fitted,
                              n_mcsim = n_sim, type = "emerging",
                              theta_col = 1.5)
  expect_equal(destandardize(scanres2), scanres)


  # scan_eb_negbin actually gives an error if baselines is not provided
  # scanres <- scan_eb_negbin(wide_cases_lg, zones_lg, baselines = wide_baseline_lg,
  #                           n_mcsim = n_sim,
  #                           thetas = 1.5)
  # scanres2 <- scan_eb_negbin2(spacetime_data_lg, cases, zones_lg,
  #                             n_mcsim = n_sim,
  #                             theta_col = 1.5)
  # expect_equal(destandardize(scanres2), scanres)

  scanres <- scan_eb_negbin(wide_cases_lg, zones_lg, baseline = wide_baseline_lg,
                            n_mcsim = n_sim,
                            thetas = wide_pop_lg)
  scanres2 <- scan_eb_negbin2(spacetime_data_lg, cases, zones_lg, baseline_col = .fitted,
                              n_mcsim = n_sim,
                              theta_col = pop)
  expect_equal(destandardize(scanres2), scanres)
})

test_that("scan_eb_zip matches", {
  # Baseline, no pop
  scanres <- scan_eb_zip(wide_cases_lg, zones_lg, wide_baseline_lg,
                         n_mcsim = n_sim,
                         probs = wide_probs_lg)

  scanres2 <- scan_eb_zip2(spacetime_data_lg, cases, zones_lg, baseline_col = .fitted,
                              n_mcsim = n_sim,
                              prob_col = probs)
  expect_equal(destandardize(scanres2), scanres)

  # No baseline, yes pop
  scanres <- scan_eb_zip(wide_cases_lg, zones_lg, population = wide_pop_lg,
                         n_mcsim = n_sim,
                         probs = wide_probs_lg)

  scanres2 <- scan_eb_zip2(spacetime_data_lg, cases, zones_lg, pop_col = pop,
                           n_mcsim = n_sim,
                           prob_col = probs)
  expect_equal(destandardize(scanres2), scanres)

  # Probs no
  scanres <- scan_eb_zip(wide_cases_lg, zones_lg, wide_baseline_lg,
                         n_mcsim = n_sim)

  scanres2 <- scan_eb_zip2(spacetime_data_lg, cases, zones_lg, baseline_col = .fitted,
                           n_mcsim = n_sim)
  expect_equal(destandardize(scanres2), scanres)

  # Probs Scalar (not allowed for scan_eb_zip)
  scanres <- scan_eb_zip(wide_cases_lg, zones_lg, wide_baseline_lg,
                         n_mcsim = n_sim,
                         probs = wide_probs_lg)

  scanres2 <- scan_eb_zip2(spacetime_data_lg, cases, zones_lg, baseline_col = .fitted,
                           n_mcsim = n_sim,
                           prob_col = 0.1)
  expect_equal(destandardize(scanres2), scanres)
})

test_that("scan_permutation matches", {
  scanres <- scan_permutation(wide_cases_lg, zones_lg, population = wide_pop_lg,
                         n_mcsim = n_sim)

  scanres2 <- scan_permutation2(spacetime_data_lg, cases, zones_lg, pop_col = pop,
                                n_mcsim = n_sim)
  expect_equal(destandardize(scanres2), scanres)

  scanres <- scan_permutation(wide_cases_lg, zones_lg,
                              n_mcsim = n_sim)
  scanres2 <- scan_permutation2(spacetime_data_lg, cases, zones_lg,
                                n_mcsim = n_sim)
  expect_equal(destandardize(scanres2), scanres)
})

test_that("scan_bayes_negbin matches", {
  scanres <- scan_bayes_negbin(wide_cases_lg, zones_lg, wide_baseline_lg,
                              beta_null = 2)
  scanres2 <- scan_bayes_negbin2(spacetime_data_lg, cases, zones_lg, baseline_col = .fitted,
                                 beta_null = 2)
  expect_equal(destandardize(scanres2), scanres)

  scanres <- scan_bayes_negbin(wide_cases_lg, zones_lg, population = wide_pop_lg,
                               beta_null = 2)
  scanres2 <- scan_bayes_negbin2(spacetime_data_lg, cases, zones_lg, pop_col = pop,
                                 beta_null = 2)
  expect_equal(destandardize(scanres2), scanres)

  scanres <- scan_bayes_negbin(wide_cases_lg, zones_lg, population = wide_pop_lg,
                               baseline = wide_baseline_lg,
                               beta_null = 2)
  scanres2 <- scan_bayes_negbin2(spacetime_data_lg, cases, zones_lg, pop_col = pop,
                                 baseline_col = .fitted,
                                 beta_null = 2)
  expect_equal(destandardize(scanres2), scanres)


  # Check that all the parameters get passed through OK
  scanres <- scan_bayes_negbin(wide_cases_lg, zones_lg, wide_baseline_lg,
                               outbreak_prob = 0.02, alpha_null = 1.2, beta_null = 0.8,
                               alpha_alt = 0.8, beta_alt = 1.2, inc_values = c(1.5, 2, 2.9),
                               inc_probs = c(2, 1, 3))
  scanres2 <- scan_bayes_negbin2(spacetime_data_lg, cases, zones_lg, baseline_col = .fitted,
                                 outbreak_prob = 0.02, alpha_null = 1.2, beta_null = 0.8,
                                 alpha_alt = 0.8, beta_alt = 1.2, inc_values = c(1.5, 2, 2.9),
                                 inc_probs = c(2, 1, 3))
  expect_equal(destandardize(scanres2), scanres)
})

test_that("scan_eb_poisson_fast matches", {
  scanres <- scan_eb_poisson_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                  n_mcsim = n_sim)
  scanres2 <- scan_eb_poisson_fast2(spacetime_data_lg, cases, zones_lg, .fitted,
                                    n_mcsim = n_sim)
  expect_equal(destandardize(scanres2), scanres)

  # I think there's no options in the inputs for scan_eb_poisson_fast, so that's all we need to check.
})

test_that("scan_eb_negbin_fast matches", {
  scanres <- scan_eb_negbin_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                            n_mcsim = n_sim,
                            thetas = 2.5)
  scanres2 <- scan_eb_negbin_fast2(spacetime_data_lg, cases, zones_lg, .fitted,
                                   theta_col = 2.5,
                                   n_mcsim = n_sim)
  expect_equal(destandardize(scanres2), scanres)


  scanres <- scan_eb_negbin_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                 n_mcsim = n_sim,
                                 thetas = wide_thetas_lg)
  scanres2 <- scan_eb_negbin_fast2(spacetime_data_lg, cases, zones_lg, .fitted,
                                   theta_col = thetas,
                                   n_mcsim = n_sim)
  expect_equal(destandardize(scanres2), scanres)
})

test_that("scan_cusum_poisson matches", {
  scanres <- scan_cusum_poisson(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                scaling = 2.5,
                                n_mcsim = n_sim)
  scanres2 <- scan_cusum_poisson2(spacetime_data_lg, cases, zones_lg, .fitted,
                                 scaling = 2.5,
                                 n_mcsim = n_sim)
  expect_equal(destandardize(scanres2), scanres)

  # And I don't think there are any options in the arguments.
})

test_that("parallel_cusum_poisson matches", {
  scanres <- parallel_cusum_poisson(wide_cases_lg, wide_baseline_lg, scaling = 1.25)
  scanres_gen <- standardized_alarm_functions("parallel_cusum_poisson",
                                              wide_cases_lg,
                                              NULL,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim,
                                              scaling = 1.25)
  expect_equal(destandardize(scanres_gen), scanres)

})
#
# test_that("parallel_cusum_gaussian matches", {
#   scanres <- parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg,
#                                      mean = 0,
#                                      sigma = c(1:9))
#   scanres_gen <- standardized_alarm_functions("parallel_cusum_gaussian",
#                                               wide_cases_lg,
#                                               NULL,
#                                               wide_baseline_lg,
#                                               n_mcsim = n_sim,
#                                               mean = 0,
#                                               sigma = 1:9)
#   expect_equal(destandardize(scanres_gen), scanres)
#
# })
#
# test_that("parallel_shewhart_gaussian matches", {
#   scanres <- parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg,
#                                      mean = 0,
#                                      sigma = c(1:9))
#   scanres_gen <- standardized_alarm_functions("parallel_shewhart_gaussian",
#                                               wide_cases_lg,
#                                               NULL,
#                                               wide_baseline_lg,
#                                               n_mcsim = n_sim,
#                                               mean = 0,
#                                               sigma = 1:9)
#   expect_equal(destandardize(scanres_gen), scanres)
#
# })
#
