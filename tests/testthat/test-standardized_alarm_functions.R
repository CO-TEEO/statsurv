set.seed(373953261)
library("scanstatistics")
source("build_data_for_alarm_functions.R")
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

  scanres_gen <- standardized_alarm_functions("scan_eb_poisson",
                                              wide_cases_lg,
                                              zones_lg,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim)
  expect_equal(destandardize(scanres_gen), scanres)
})

test_that("scan_pb_poisson matches", {
  scanres <- scan_pb_poisson(wide_cases_lg, zones_lg, population = wide_baseline_lg,
                             n_mcsim = n_sim,
                             gumbel = TRUE)

  scanres_gen <- standardized_alarm_functions("scan_pb_poisson",
                                              wide_cases_lg,
                                              zones_lg,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim,
                                              gumbel = TRUE)
  expect_equal(destandardize(scanres_gen), scanres)
})

test_that("scan_eb_negbin matches", {
  scanres <- scan_eb_negbin(wide_cases_lg, zones_lg, baseline = wide_baseline_lg,
                             n_mcsim = n_sim,
                             thetas = 1.5)

  scanres_gen <- standardized_alarm_functions("scan_eb_negbin",
                                              wide_cases_lg,
                                              zones_lg,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim,
                                              thetas = 1.5)
  expect_equal(destandardize(scanres_gen), scanres)

})

test_that("scan_eb_zip matches", {
  wide_probs <- matrix(0.1, nrow = nrow(wide_cases_lg), ncol = ncol(wide_cases_lg))
  scanres <- scan_eb_zip(wide_cases_lg, zones_lg, wide_baseline_lg,
                             n_mcsim = n_sim,
                             probs = wide_probs)

  scanres_gen <- standardized_alarm_functions("scan_eb_zip",
                                              wide_cases_lg,
                                              zones_lg,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim,
                                              probs = wide_probs)
  expect_equal(destandardize(scanres_gen), scanres)

})

test_that("scan_permutation matches", {
  scanres <- scan_permutation(wide_cases_lg, zones_lg, wide_baseline_lg,
                         n_mcsim = n_sim)

  scanres_gen <- standardized_alarm_functions("scan_permutation",
                                              wide_cases_lg,
                                              zones_lg,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim)
  expect_equal(destandardize(scanres_gen), scanres)

})

test_that("scan_bayes_negbin matches", {
  scanres <- scan_bayes_negbin(wide_cases_lg, zones_lg, wide_baseline_lg,
                              beta_null = 2)

  scanres_gen <- standardized_alarm_functions("scan_bayes_negbin",
                                              wide_cases_lg,
                                              zones_lg,
                                              wide_baseline_lg,
                                              beta_null = 2)
  expect_equal(destandardize(scanres_gen), scanres)

})

test_that("scan_eb_poisson_fast matches", {
  scanres <- scan_eb_poisson_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                  n_mcsim = n_sim)

  scanres_gen <- standardized_alarm_functions("scan_eb_poisson_fast",
                                              wide_cases_lg,
                                              key_matrix_lg,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim)
  expect_equal(destandardize(scanres_gen), scanres)

})

test_that("scan_eb_negbin_fast matches", {
  scanres <- scan_eb_negbin_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                            n_mcsim = n_sim,
                            thetas = 1.5)

  scanres_gen <- standardized_alarm_functions("scan_eb_negbin_fast",
                                              wide_cases_lg,
                                              key_matrix_lg,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim,
                                              thetas = 1.5)
  expect_equal(destandardize(scanres_gen), scanres)

})

test_that("scan_cusum_poisson matches", {
  scanres <- scan_cusum_poisson(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                                scaling = 2.5,
                                n_mcsim = n_sim)

  scanres_gen <- standardized_alarm_functions("scan_cusum_poisson",
                                              wide_cases_lg,
                                              key_matrix_lg,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim,
                                              scaling = 2.5)
  expect_equal(destandardize(scanres_gen), scanres)

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

test_that("parallel_cusum_gaussian matches", {
  scanres <- parallel_cusum_gaussian(wide_cases_lg, wide_baseline_lg,
                                     mean = 0,
                                     sigma = c(1:9))
  scanres_gen <- standardized_alarm_functions("parallel_cusum_gaussian",
                                              wide_cases_lg,
                                              NULL,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim,
                                              mean = 0,
                                              sigma = 1:9)
  expect_equal(destandardize(scanres_gen), scanres)

})

test_that("parallel_shewhart_gaussian matches", {
  scanres <- parallel_shewhart_gaussian(wide_cases_lg, wide_baseline_lg,
                                     mean = 0,
                                     sigma = c(1:9))
  scanres_gen <- standardized_alarm_functions("parallel_shewhart_gaussian",
                                              wide_cases_lg,
                                              NULL,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim,
                                              mean = 0,
                                              sigma = 1:9)
  expect_equal(destandardize(scanres_gen), scanres)

})

test_that("We can swap zones and key_matrix in the general", {
  scanres <- scan_eb_poisson(wide_cases_lg, zones_lg, baseline = wide_baseline_lg,
                             n_mcsim = n_sim)

  scanres_gen1 <- standardized_alarm_functions("scan_eb_poisson",
                                              wide_cases_lg,
                                              zones_lg,
                                              wide_baseline_lg,
                                              n_mcsim = n_sim)
  expect_equal(destandardize(scanres_gen1), scanres)


  scanres_gen2 <- standardized_alarm_functions("scan_eb_poisson",
                                               wide_cases_lg,
                                               key_matrix_lg,
                                               wide_baseline_lg,
                                               n_mcsim = n_sim)
  expect_equal(destandardize(scanres_gen2), scanres)

})

test_that("We can swap key_matrix and zones in the general", {
  scanres <- scan_eb_poisson_fast(wide_cases_lg, key_matrix_lg, wide_baseline_lg,
                             n_mcsim = n_sim)

  scanres_gen1 <- standardized_alarm_functions("scan_eb_poisson_fast",
                                               wide_cases_lg,
                                               zones_lg,
                                               wide_baseline_lg,
                                               n_mcsim = n_sim)
  expect_equal(destandardize(scanres_gen1), scanres)


  scanres_gen2 <- standardized_alarm_functions("scan_eb_poisson_fast",
                                               wide_cases_lg,
                                               key_matrix_lg,
                                               wide_baseline_lg,
                                               n_mcsim = n_sim)
  expect_equal(destandardize(scanres_gen2), scanres)

})







