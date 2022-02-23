#' @inherit scanstatistics::scan_eb_poisson title
#'
#' @description Calculate the expectation-based Poisson scan statistic using data frames.
#'   `scan_eb_poisson2` is a helpful wrapper around \code{\link[scanstatistics]{scan_eb_poisson}}
#'   allowing data to be passed as a single long data frame, instead of as a series of wide
#'   matrices.
#'
#' @inheritParams spacetime_data
#' @inheritParams parallel_cusum_gaussian2
#' @inheritParams scanstatistics::scan_eb_poisson
#' @param baseline_col The column in data containing baseline estimates of the outcome of interest.
#'   Optional if `pop_col` is provided, otherwise required.
#' @param pop_col Optional. The column in `spacetime_data` containing the population for each
#'   location/time. Used to estimate the baseline if `baseline_col` is NULL, otherwise ignored.
#'
#' @return An object of class `scanstatistics`, containing information about the type of scan
#'   statistic, the most likely cluster (MLC), and calculated scan statistics for every zone and
#'   duration investigated.
#' @export
#' @md
#'
#' @family v2 scan statistics
#' @seealso \code{\link[scanstatistics]{scan_eb_poisson}}
#' @examples
#' library(sf)
#' data("NM_data")
#' data("NM_county_sf")
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' scan_eb_poisson2(NM_data, outcome_col = count, zones,
#'                  baseline_col = baseline_est, n_mcsim = 100)
#' scan_eb_poisson2(NM_data, outcome_col = count, zones,
#'                  pop_col = population, n_mcsim = 100, gumbel = TRUE)
scan_eb_poisson2 <- function(spacetime_data, outcome_col, zones,
                             baseline_col = NULL, pop_col = NULL,
                             n_mcsim = 10, gumbel = FALSE, max_only = FALSE) {

  # Arg checks:
  validate_spacetime_data(spacetime_data)
  validate_zones(zones, spacetime_data)

  # Code
  wide_cases <- pivot_for_scan(spacetime_data, {{outcome_col}})

  if (!missing(baseline_col)) {
    wide_baseline <- pivot_for_scan(spacetime_data, {{baseline_col}})
  } else {
    wide_baseline <- NULL
  }

  if (!missing(pop_col)) {
    wide_pop <- pivot_for_scan(spacetime_data, {{pop_col}})
  } else {
    wide_pop <- NULL
  }

  alarm_res <- scanstatistics::scan_eb_poisson(wide_cases, zones,
                                               baselines = wide_baseline, population = wide_pop,
                                               n_mcsim = n_mcsim, gumbel = gumbel,
                                               max_only = max_only)

  standardize_scan_alarm(alarm_res, zones, gumbel = gumbel)
}

#' @inherit scan_eb_poisson_fast title
#'
#' @description Calculate the expectation-based scan statistic using the a Poisson distribution, as
#'   devised by Neill (2009). `scan_eb_poisson_fast2` is a helpful wrapper around
#'   \code{\link{scan_eb_poisson_fast}} allowing data to be passed as a single long data frame,
#'   instead of as a series of wide matrices.
#'
#' @inheritParams scan_eb_poisson2
#' @param baseline_col The column in data containing baseline estimates of the outcome of interest.
#'   Required for `scan_eb_poisson_fast2`.
#'
#' @inherit scan_eb_poisson2 return
#' @export
#' @md
#'
#' @family v2 scan statistics
#' @seealso \code{\link{scan_eb_poisson_fast}}
#'
#' @examples
#' library(sf)
#' data("NM_data")
#' data("NM_county_sf")
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' scan_eb_poisson_fast2(NM_data, outcome_col = count, zones,
#'                       baseline_col = baseline_est, n_mcsim = 99)
scan_eb_poisson_fast2 <- function(spacetime_data, outcome_col, zones, baseline_col, n_mcsim = 10) {

  # Arg checks:
  validate_spacetime_data(spacetime_data)
  validate_zones(zones, spacetime_data)

  # Code
  wide_baseline <- pivot_for_scan(spacetime_data,
                                  {{baseline_col}})
  wide_cases <- pivot_for_scan(spacetime_data,
                               {{outcome_col}})

  key_matrix <- zones_to_key_matrix(zones)

  alarm_res <- scan_eb_poisson_fast(wide_cases, key_matrix, wide_baseline, n_mcsim = n_mcsim)
  standardize_scan_alarm(alarm_res, zones)
}

#' @inherit scan_eb_negbin_fast title
#'
#' @description Calculate the expectation-based scan statistic using the a negative binomial
#'   distribution, as devised by Tango et al. (2011). Note that although this implementation is
#'   based on that given in the `scanstatistics package`, it gives different results.
#'   `scan_eb_negbin_fast2` is a helpful wrapper around \code{\link{scan_eb_negbin_fast}} allowing
#'   data to be passed as a single long data frame, instead of as a series of wide matrices.
#'
#' @inheritParams scan_eb_poisson_fast2
#' @param theta_col The dispersion parameter of the negative binomial distribution. As the value of
#'   theta increases, the negative binomial distribution approaches the Poisson distribution. This
#'   parameter is typically estimates from past data using a model. Can be specified either as a
#'   scalar or as a column in `spacetime_data`.
#' @param baseline_col The column in data containing baseline estimates of the outcome of interest.
#'   Required for `scan_eb_negbin_fast2`.
#'
#' @inherit scan_eb_poisson2 return
#' @export
#' @md
#'
#' @family v2 scan statistics
#' @seealso \code{\link{scan_eb_negbin_fast}}
#' @examples
#' library(sf)
#' data("NM_data")
#' data("NM_county_sf")
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' scan_eb_negbin_fast2(NM_data, outcome_col = count, zones,
#'                      baseline_col = baseline_est, theta_col = 1,
#'                      n_mcsim = 99)
#'
#' NM_data$thetas <- sqrt(NM_data$count) + 0.1
#' scan_eb_negbin_fast2(NM_data, outcome_col = count, zones,
#'                      baseline_col = baseline_est, theta_col = thetas,
#'                      n_mcsim = 99)
#'
scan_eb_negbin_fast2 <- function(spacetime_data, outcome_col, zones, baseline_col,
                                 theta_col = 1, n_mcsim = 0) {

  # Arg checks:
  validate_spacetime_data(spacetime_data)
  validate_zones(zones, spacetime_data)

  # Code
  wide_baseline <- pivot_for_scan(spacetime_data, {{baseline_col}})
  wide_cases <- pivot_for_scan(spacetime_data, {{outcome_col}})
  key_matrix <- zones_to_key_matrix(zones)

  wide_thetas <- pivot_for_scan(spacetime_data, {{theta_col}})

  alarm_res <- scan_eb_negbin_fast(wide_cases, key_matrix, wide_baseline, wide_thetas, n_mcsim)
  standardize_scan_alarm(alarm_res, zones)
}

#' @inherit scanstatistics::scan_eb_negbin title
#'
#' @description Calculate the expectation-based scan statistic using the a negative binomial
#'   distribution, as devised by Tango et al. (2011). `scan_eb_negbin2` is a helpful wrapper around
#'   \code{\link[scanstatistics]{scan_eb_negbin}} allowing data to be passed as a single long data
#'   frame, instead of as a series of wide matrices.
#'
#' @inheritParams scan_eb_negbin_fast2
#' @inheritParams scanstatistics::scan_eb_negbin
#' @param baseline_col The column in data containing baseline estimates of the outcome of interest.
#'   Typically estimated from past data using a model.
#'
#' @inherit scan_eb_poisson2 return
#' @export
#' @md
#'
#' @family v2 scan statistics
#' @seealso \code{\link[scanstatistics]{scan_eb_negbin}}
#'
#' @examples
#' library(sf)
#' data("NM_data")
#' data("NM_county_sf")
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' scan_eb_negbin2(NM_data, outcome_col = count, zones,
#'                 baseline_col = baseline_est, theta_col = 1,
#'                 n_mcsim = 99)
#'
#' NM_data$thetas <- sqrt(NM_data$count) + 0.1
#' scan_eb_negbin2(NM_data, outcome_col = count, zones,
#'                 baseline_col = baseline_est, theta_col = thetas,
#'                 type = "emerging",
#'                 n_mcsim = 99)
#'
scan_eb_negbin2 <- function(spacetime_data, outcome_col, zones, baseline_col, theta_col = 1,
                            type = c("hotspot", "emerging"), n_mcsim = 10, gumbel = FALSE,
                            max_only = FALSE) {

  # Arg checks:
  validate_spacetime_data(spacetime_data)
  validate_zones(zones, spacetime_data)

  # Code
  wide_cases <- pivot_for_scan(spacetime_data, {{outcome_col}})
  wide_baseline <- pivot_for_scan(spacetime_data, {{baseline_col}})

  if (!missing(theta_col)) {
    wide_theta <- pivot_for_scan(spacetime_data, {{theta_col}})
  } else {
    wide_theta <- NULL
  }

  alarm_res <- scanstatistics::scan_eb_negbin(wide_cases, zones, wide_baseline,
                                              thetas = wide_theta, type = type,
                                              n_mcsim = n_mcsim, gumbel = gumbel,
                                              max_only = max_only)
  standardize_scan_alarm(alarm_res, zones, gumbel = gumbel)
}


#' @inherit scanstatistics::scan_eb_zip title
#'
#' @description Calculates the expectation-based scan statistic with a zero-inflated Poisson
#'   distribution. `scan_eb_zip2` is a helpful wrapper around
#'   \code{\link[scanstatistics]{scan_eb_zip}} allowing data to be passed as a single long data
#'   frame, instead of as a series of wide matrices.
#'
#' @inheritParams scan_eb_poisson2
#' @inheritParams scanstatistics::scan_eb_zip
#' @param prob_col The column in `spacetime_data` containing the structural zero probability of the
#'   ZIP distribution for each space and time. Can alternatively be specified as a scalar, if the
#'   structural zero probability is a constant. Optional; if not provided, with be estimated
#'   heuristically.
#' @param pop_col Optional. The column in `spacetime_data` containing the population for each
#'   location/time. Used to estimate the baseline or strucutural zero probabilitiy if `baseline_col`
#'   or `prob_col` is NULL, otherwise ignored.
#'
#' @inherit scan_eb_poisson2 return
#' @export
#' @md
#'
#' @family v2 scan statistics
#' @seealso \code{\link[scanstatistics]{scan_eb_zip}}
#' @examples
#' library(sf)
#' data("NM_data")
#' data("NM_county_sf")
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' scan_eb_zip2(NM_data, outcome_col = count, zones,
#'              baseline_col = baseline_est, prob_col = NULL,
#'              n_mcsim = 99)
#'
#' scan_eb_zip2(NM_data, outcome_col = count, zones,
#'              baseline_col = baseline_est, prob_col = 0.2,
#'              n_mcsim = 99)
#'
scan_eb_zip2 <- function(spacetime_data, outcome_col, zones,
                         baseline_col = NULL, prob_col = NULL, pop_col = NULL,
                         n_mcsim = 10, gumbel = FALSE,
                         max_only = FALSE, rel_tol = 0.001) {

  # Arg checks:
  validate_spacetime_data(spacetime_data)
  validate_zones(zones, spacetime_data)

  # Code

  wide_cases <- pivot_for_scan(spacetime_data, {{outcome_col}})
  if (!missing(baseline_col)) {
    wide_baseline <- pivot_for_scan(spacetime_data, {{baseline_col}})
  } else {
    wide_baseline <- NULL
  }

  if (!missing(prob_col)) {
    wide_probs <- pivot_for_scan(spacetime_data, {{prob_col}})
  } else {
    wide_probs <- NULL
  }

  if (!missing(pop_col)) {
    wide_pop <- pivot_for_scan(spacetime_data, {{pop_col}})
  } else {
    wide_pop <- NULL
  }


  alarm_res <- scanstatistics::scan_eb_zip(wide_cases, zones,
                                           wide_baseline, wide_probs, wide_pop,
                                           n_mcsim = n_mcsim, gumbel = gumbel,
                                           max_only = max_only, rel_tol = rel_tol)
  standardize_scan_alarm(alarm_res, zones, gumbel = gumbel)
}

#' @inherit scanstatistics::scan_pb_poisson title
#'
#' @description Calculate the population-based Poisson scan statistic devised by Kulldorff (1997,
#'   2001). `scan_pb_poisson2` is a helpful wrapper around
#'   \code{\link[scanstatistics]{scan_pb_poisson}} allowing data to be passed as a single long data
#'   frame, instead of as a series of wide matrices.
#'
#' @inheritParams scan_eb_poisson2
#' @param pop_col Optional. The column in `spacetime_data` containing the population for each
#'   location/time. Only necessary if you want to account for the different population in each
#'   location and time.
#'
#' @inherit scan_eb_poisson2 return
#'
#' @family v2 scan statistics
#' @seealso \code{\link[scanstatistics]{scan_pb_poisson}}
#' @export
#' @md
#'
#' @examples
#' library(sf)
#' data("NM_data")
#' data("NM_county_sf")
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' scan_pb_poisson2(NM_data, outcome_col = count, zones,
#'                  n_mcsim = 99)
#'
#' scan_pb_poisson2(NM_data, outcome_col = count, zones,
#'                  pop_col = population,
#'                  n_mcsim = 99)
#'
scan_pb_poisson2 <- function(spacetime_data, outcome_col, zones, pop_col = NULL, n_mcsim = 10,
                             gumbel = FALSE,
                             max_only = FALSE) {

  # Arg checks:
  validate_spacetime_data(spacetime_data)
  validate_zones(zones, spacetime_data)

  # Code
  wide_cases <- pivot_for_scan(spacetime_data, {{outcome_col}})
  if (!missing(pop_col)) {
    wide_pop <- pivot_for_scan(spacetime_data, {{pop_col}})
  } else {
    wide_pop <- NULL
  }
  alarm_res <- scanstatistics::scan_pb_poisson(wide_cases, zones, wide_pop,
                                               n_mcsim = n_mcsim, gumbel = gumbel,
                                               max_only = max_only)
  standardize_scan_alarm(alarm_res, zones, gumbel = gumbel)
}

#' @inherit scanstatistics::scan_permutation title
#'
#' @description Calculate the space-time permutation scan statistic devised by Kulldorff (2005).
#'   `scan_permutation2` is a helpful wrapper around \code{\link[scanstatistics]{scan_permutation}}
#'   allowing data to be passed as a single long data frame, instead of as a series of wide
#'   matrices.
#'
#' @inheritParams scan_pb_poisson2
#'
#' @inherit scan_eb_poisson2 return
#' @export
#' @md
#'
#' @family v2 scan statistics
#' @seealso \code{\link[scanstatistics]{scan_permutation}}
#' @examples
#' library(sf)
#' data("NM_data")
#' data("NM_county_sf")
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' scan_permutation2(NM_data, outcome_col = count, zones,
#'                   n_mcsim = 99)
#'
#' scan_permutation2(NM_data, outcome_col = count, zones,
#'                   pop_col = population,
#'                   n_mcsim = 99)
#'
scan_permutation2 <- function(spacetime_data, outcome_col, zones, pop_col = NULL, n_mcsim = 0,
                              gumbel = FALSE,
                              max_only = FALSE) {

  # Arg checks:
  validate_spacetime_data(spacetime_data)
  validate_zones(zones, spacetime_data)

  # Code
  wide_cases <- pivot_for_scan(spacetime_data, {{outcome_col}})
  if (!missing(pop_col)) {
    wide_pop <- pivot_for_scan(spacetime_data, {{pop_col}})
  } else {
    wide_pop <- NULL
  }

  alarm_res <- scanstatistics::scan_permutation(wide_cases, zones, population = wide_pop,
                                                n_mcsim = n_mcsim, gumbel = gumbel,
                                                max_only = max_only)
  standardize_scan_alarm(alarm_res, zones, gumbel = gumbel)
}


#' @inherit scanstatistics::scan_bayes_negbin title
#'
#' @description Calculate the "Bayesian Spatial Scan Statistic" by Neill et al. (2006), adapted to a
#'   spatio-temporal setting. `scan_bayes_negbin2` is a  helpful wrapper around
#'   \code{\link[scanstatistics]{scan_bayes_negbin}} allowing data to be passed as a single long
#'   data frame, instead of as a series of wide matrices.
#'
#' @inheritParams scan_eb_poisson2
#' @inheritParams scanstatistics::scan_bayes_negbin
#'
#' @inherit scan_eb_poisson2 return
#' @export
#' @md
#'
#' @family v2 scan statistics
#' @seealso \code{\link[scanstatistics]{scan_bayes_negbin}}
#'
#' @examples
#' library(sf)
#' data("NM_data")
#' data("NM_county_sf")
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' scan_bayes_negbin2(NM_data, outcome_col = count, zones,
#'                    baseline_col = baseline_est)
#'
#' scan_bayes_negbin2(NM_data, outcome_col = count, zones,
#'                    pop_col = population)
#'
#' scan_bayes_negbin2(NM_data, outcome_col = count, zones,
#'                    baseline_col = baseline_est,
#'                    outbreak_prob = 0.1)
#'
scan_bayes_negbin2 <- function(spacetime_data, outcome_col, zones,
                               baseline_col = NULL, pop_col = NULL,
                               outbreak_prob = 0.05, alpha_null = 1, beta_null = 1,
                               alpha_alt = alpha_null, beta_alt = beta_null,
                               inc_values = seq(1, 3, by = 0.1),
                               inc_probs = 1) {

  # Arg checks:
  validate_spacetime_data(spacetime_data)
  validate_zones(zones, spacetime_data)

  # Code
  wide_cases <- pivot_for_scan(spacetime_data, {{outcome_col}})

  if (!missing(baseline_col)) {
    wide_baseline <- pivot_for_scan(spacetime_data, {{baseline_col}})
  } else {
    wide_baseline <- NULL
  }

  if (!missing(pop_col)) {
    wide_pop <- pivot_for_scan(spacetime_data, {{pop_col}})
  } else {
    wide_pop <- NULL
  }

  alarm_res <- scanstatistics::scan_bayes_negbin(wide_cases, zones,
                                                 wide_baseline, wide_pop,
                                                 outbreak_prob = outbreak_prob,
                                                 alpha_null = alpha_null, beta_null = beta_null,
                                                 alpha_alt = alpha_alt, beta_alt = beta_alt,
                                                 inc_values = inc_values, inc_probs = inc_probs)

  attr(alarm_res, "alarm_type") <- "scan"
  alarm_res$zone_info <- zones
  alarm_res$observed <- alarm_res$posteriors$window_posteriors
  alarm_res$observed$action_level <- alarm_res$observed$log_posterior
  alarm_res$MLC$action_level <- alarm_res$MLC$log_posterior

  alarm_res$posteriors$window_posteriors$action_level <-
    alarm_res$posteriors$window_posteriors$log_posterior
  alarm_res
}


#' @inherit scan_cusum_poisson title
#'
#' @description Calculate the CUSUM alarm statistic over a large set of spatio-temporal zones, as
#'   described by Sonesson (2007). `scan_cusum_poisson2` is a helpful wrapper around
#'   \code{\link{scan_cusum_poisson}} allowing data to be passed as a single long data frame,
#'   instead of as a series of wide matrices.
#'
#' @inheritParams scan_eb_poisson2
#' @inheritParams scan_cusum_poisson
#'
#' @inherit scan_eb_poisson2 return
#' @export
#' @md
#' @family v2 scan statistics
#' @seealso \code{\link{scan_cusum_poisson}} \code{\link{parallel_cusum_poisson}}
#' @examples
#' library(sf)
#' data("NM_data")
#' data("NM_county_sf")
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' scan_cusum_poisson2(NM_data, outcome_col = count, zones,
#'                     baseline_col = baseline_est)
#'
#' scan_cusum_poisson2(NM_data, outcome_col = count, zones,
#'                     baseline_col = baseline_est,
#'                     scaling = 1.25,
#'                     n_mcsim = 99)
#'
scan_cusum_poisson2 <- function(spacetime_data, outcome_col, zones, baseline_col, scaling = 1.5,
                                n_mcsim = 10) {
  # Arg checks:
  validate_spacetime_data(spacetime_data)
  validate_zones(zones, spacetime_data)

  # Code
  wide_cases <- pivot_for_scan(spacetime_data, {{outcome_col}})
  wide_baseline <- pivot_for_scan(spacetime_data, {{baseline_col}})

  key_matrix <- zones_to_key_matrix(zones)
  alarm_res <- scan_cusum_poisson(wide_cases, key_matrix, wide_baseline,
                                  scaling = scaling, n_mcsim = n_mcsim)
  standardize_scan_alarm(alarm_res, zones)
}

standardize_scan_alarm <- function(alarm_res, zones, gumbel = FALSE) {
  attr(alarm_res, "alarm_type") <- "scan"
  alarm_res$zone_info <- zones
  alarm_res$observed$action_level <- alarm_res$observed$score
  alarm_res$MLC$action_level <- alarm_res$MLC$score
  alarm_res$replicates$action_level <- alarm_res$replicates$score
  alarm_res$observed$mc_pvalue <- mc_pvalue(alarm_res$observed$action_level,
                                            alarm_res$replicates$action_level)

  if (gumbel) {
    alarm_res$observed$gumbel_pvalue <-
      gumbel_pvalue(alarm_res$observed$action_level,
                    alarm_res$replicates$action_level)$pvalue
  }
  alarm_res
}



