scan_eb_poisson2 <- function(data, outcome_col, zones, baseline_col = NULL, pop_col = NULL,
                             n_mcsim = 10, gumbel = FALSE, max_only = FALSE) {

  # Arg checks:
  validate_spacetime_data(data)
  validate_zones(zones, data)

  # Code
  wide_cases <- pivot_for_scan(data, {{outcome_col}})

  if (!missing(baseline_col)) {
    wide_baseline <- pivot_for_scan(data, {{baseline_col}})
  } else {
    wide_baseline <- NULL
  }

  if (!missing(pop_col)) {
    wide_pop <- pivot_for_scan(data, {{pop_col}})
  } else {
    wide_pop <- NULL
  }

  alarm_res <- scanstatistics::scan_eb_poisson(wide_cases, zones,
                                               baselines = wide_baseline, population = wide_pop,
                                               n_mcsim = n_mcsim, gumbel = gumbel,
                                               max_only = max_only)

  standardize_scan_alarm(alarm_res, zones)
}

scan_eb_poisson_fast2 <- function(data, outcome_col, zones, baseline_col, n_mcsim = 10) {

  # Arg checks:
  validate_spacetime_data(data)
  validate_zones(zones, data)

  # Code
  wide_baseline <- pivot_for_scan(data,
                                  {{baseline_col}})
  wide_cases <- pivot_for_scan(data,
                               {{outcome_col}})

  key_matrix <- zones_to_key_matrix(zones)

  alarm_res <- scan_eb_poisson_fast(wide_cases, key_matrix, wide_baseline, n_mcsim = n_mcsim)
  standardize_scan_alarm(alarm_res, zones)
}

scan_eb_negbin_fast2 <- function(data, outcome_col, zones, baseline_col,
                                 theta_col = 1, n_mcsim = 0) {

  # Arg checks:
  validate_spacetime_data(data)
  validate_zones(zones, data)

  # Code
  wide_baseline <- pivot_for_scan(data, {{baseline_col}})
  wide_cases <- pivot_for_scan(data, {{outcome_col}})
  key_matrix <- zones_to_key_matrix(zones)

  wide_thetas <- pivot_for_scan(data, {{theta_col}})

  alarm_res <- scan_eb_negbin_fast(wide_cases, key_matrix, wide_baseline, wide_thetas, n_mcsim)
  standardize_scan_alarm(alarm_res, zones)
}

scan_eb_negbin2 <- function(data, outcome_col, zones, baseline_col, theta_col = 1,
                            type = c("hotspot", "emerging"), n_mcsim = 10, gumbel = FALSE,
                            max_only = FALSE) {

  # Arg checks:
  validate_spacetime_data(data)
  validate_zones(zones, data)

  # Code
  wide_cases <- pivot_for_scan(data, {{outcome_col}})
  wide_baseline <- pivot_for_scan(data, {{baseline_col}})

  if (!missing(theta_col)) {
    wide_theta <- pivot_for_scan(data, {{theta_col}})
  } else {
    wide_theta <- NULL
  }

  alarm_res <- scanstatistics::scan_eb_negbin(wide_cases, zones, wide_baseline,
                                              thetas = wide_theta, type = type,
                                              n_mcsim = n_mcsim, gumbel = gumbel, max_only = max_only)
  standardize_scan_alarm(alarm_res, zones)
}


scan_eb_zip2 <- function(data, outcome_col, zones,
                         baseline_col = NULL, prob_col = NULL, pop_col = NULL,
                         n_mcsim = 10, gumbel = FALSE,
                         max_only = FALSE, rel_tol = 0.001) {

  # Arg checks:
  validate_spacetime_data(data)
  validate_zones(zones, data)

  # Code

  wide_cases <- pivot_for_scan(data, {{outcome_col}})
  if (!missing(baseline_col)) {
    wide_baseline <- pivot_for_scan(data, {{baseline_col}})
  } else {
    wide_baseline <- NULL
  }

  if (!missing(prob_col)) {
    wide_probs <- pivot_for_scan(data, {{prob_col}})
  } else {
    wide_probs <- NULL
  }

  if (!missing(pop_col)) {
    wide_pop <- pivot_for_scan(data, {{pop_col}})
  } else {
    wide_pop <- NULL
  }


  alarm_res <- scanstatistics::scan_eb_zip(wide_cases, zones,
                                           wide_baseline, wide_probs, wide_pop,
                                           n_mcsim = n_mcsim, gumbel = gumbel,
                                           max_only = max_only, rel_tol = rel_tol)
  standardize_scan_alarm(alarm_res, zones)
}

scan_pb_poisson2 <- function(data, outcome_col, zones, pop_col = NULL, n_mcsim = 10, gumbel = FALSE,
                             max_only = FALSE) {

  # Arg checks:
  validate_spacetime_data(data)
  validate_zones(zones, data)

  # Code
  wide_cases <- pivot_for_scan(data, {{outcome_col}})
  if (!missing(pop_col)) {
    wide_pop <- pivot_for_scan(data, {{pop_col}})
  } else {
    wide_pop <- NULL
  }
  alarm_res <- scanstatistics::scan_pb_poisson(wide_cases, zones, wide_pop,
                                               n_mcsim = n_mcsim, gumbel = gumbel,
                                               max_only = max_only)
  standardize_scan_alarm(alarm_res, zones)
}

scan_permutation2 <- function(data, outcome_col, zones, pop_col = NULL, n_mcsim = 0, gumbel = FALSE,
                              max_only = FALSE) {

  # Arg checks:
  validate_spacetime_data(data)
  validate_zones(zones, data)

  # Code
  wide_cases <- pivot_for_scan(data, {{outcome_col}})
  if (!missing(pop_col)) {
    wide_pop <- pivot_for_scan(data, {{pop_col}})
  } else {
    wide_pop <- NULL
  }

  alarm_res <- scanstatistics::scan_permutation(wide_cases, zones, population = wide_pop,
                                                n_mcsim = n_mcsim, gumbel = gumbel,
                                                max_only = max_only)
  standardize_scan_alarm(alarm_res, zones)
}

scan_bayes_negbin2 <- function(data, outcome_col, zones, baseline_col = NULL, pop_col = NULL,
                               outbreak_prob = 0.05, alpha_null = 1, beta_null = 1,
                               alpha_alt = alpha_null, beta_alt = beta_null,
                               inc_values = seq(1, 3, by = 0.1),
                               inc_probs = 1) {

  # Arg checks:
  validate_spacetime_data(data)
  validate_zones(zones, data)

  # Code
  wide_cases <- pivot_for_scan(data, {{outcome_col}})

  if (!missing(baseline_col)) {
    wide_baseline <- pivot_for_scan(data, {{baseline_col}})
  } else {
    wide_baseline <- NULL
  }

  if (!missing(pop_col)) {
    wide_pop <- pivot_for_scan(data, {{pop_col}})
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
  alarm_res
}

scan_cusum_poisson2 <- function(data, outcome_col, zones, baseline_col, scaling = 1.5, n_mcsim = 10) {
  # Arg checks:
  validate_spacetime_data(data)
  validate_zones(zones, data)

  # Code
  wide_cases <- pivot_for_scan(data, {{outcome_col}})
  wide_baseline <- pivot_for_scan(data, {{baseline_col}})

  key_matrix <- zones_to_key_matrix(zones)
  alarm_res <- scan_cusum_poisson(wide_cases, key_matrix, wide_baseline,
                                  scaling = scaling, n_mcsim = n_mcsim)
  standardize_scan_alarm(alarm_res, zones)
}

standardize_scan_alarm <- function(alarm_res, zones) {
  attr(alarm_res, "alarm_type") <- "scan"
  alarm_res$zone_info <- zones
  alarm_res$observed$action_level <- alarm_res$observed$score
  alarm_res$MLC$action_level <- alarm_res$MLC$score
  alarm_res$replicates$action_level <- alarm_res$replicates$score
  alarm_res
}



