#' Calculate the Gaussian CUSUM statistic over multiple spatial regions
#'
#' `parallel_cusum_gaussian` calculates the Gaussian CUSUM statistic independently over each spatial
#' region, over all time points. Each spatial region is considered independently.
#'
#' @inheritParams spacetime_data
#' @param outcome_col The column in `spacetime_data` containing the outcome of interest, such as
#'   observed counts
#' @param baseline_col Optional. The column in `data` containing baseline estimates of the outcome
#'   of interest. If not present, assumed to be 0 .
#' @param mean The estimated mean of the residuals (`spacetime_data$outcome_col -
#'   spacetime_data$baseline_col`). Can either be a scalar value or a column in `spacetime_data`
#' @param sigma The estimated standard deviation of the residuals. Can either be a scalar value or a
#'   column in `spacetime_data`
#' @param drift The downward drift to use in calculating the CUSUM statistic.  The value of `drift`
#'   is typically half of the expected increase in the value of the standardized residuals during an
#'   outbreak. Can either be a scalar value or a column in `spacetime_data`
#'
#' @details The CUSUM statistic is widely used surveillance technique for purely temporal processes.
#'   It is calculated by taking the cumulative sum of the standard residuals with a downward drift
#'   and a floor of 0. For a Gaussian process, residuals are calculated as `resid = outcome -
#'   baseline`, and standard residuals are calculated as `(resid - mean)/sigma`. The CUSUM
#'   statistic is then calculated by summing the standardized residuals with downward drift `drift`.
#'
#'   The three parameters used in calculating the standardized residuals and CUSUM statistic,
#'   `mean`, `sigma`, and `drift`, can each either be a scalar or a column in `spacetime_data`.
#'
#' @return `spacetime_data` with the added column `.action_level` containing the value of the CUSUM
#'   statistic at that point and time.
#' @export
#' @md
#' @seealso parallel_cusum_gaussian
#' @family parallel2_alarms
#' @examples
#' data(NM_data)
#'
#' parallel_cusum_gaussian2(NM_data, count, drift = 1)
#' parallel_cusum_gaussian2(NM_data, count, baseline_est)
#' parallel_cusum_gaussian2(NM_data, count,
#'                          population * mean(count) / mean(population),
#'                          sigma = 2)
parallel_cusum_gaussian2 <- function(spacetime_data, outcome_col, baseline_col = NULL,
                                     mean = 0,
                                     sigma = 1,
                                     drift = 0.5) {

  validate_spacetime_data(spacetime_data)
  params <- dplyr::transmute(spacetime_data,
                             id_space = .data$id_space,
                             id_time = .data$id_time,
                             .mean = {{mean}},
                             .sigma = {{sigma}},
                             .drift = {{drift}})
  wide_mean <- pivot_for_scan(params, .data$.mean)
  wide_sigma <- pivot_for_scan(params, .data$.sigma)
  wide_drift <- pivot_for_scan(params, .data$.drift)


  wide_baseline <- pivot_for_scan(spacetime_data,
                                  {{baseline_col}})
  wide_cases <- pivot_for_scan(spacetime_data,
                               {{outcome_col}})


  alarm_res <- parallel_cusum_gaussian(wide_cases,
                                       wide_baseline,
                                       mean = wide_mean,
                                       sigma = wide_sigma,
                                       drift = wide_drift)

  alarm_res_df <- unpivot_parallel_alarms(alarm_res)
  dplyr::left_join(spacetime_data, alarm_res_df, by = c("id_space", "id_time"))
}


#' Calculate the Poisson CUSUM statistic over multiple spatial regions
#'
#' `parallel_cusum_poisson` calculates the Poisson CUSUM statistic independently over each spatial
#' region, over all time points. Each spatial region is considered independently.
#'
#'
#' @inheritParams spacetime_data
#' @inheritParams parallel_cusum_gaussian2
#' @param baseline_col The column in `data` containing baseline estimates of the outcome
#'   of interest.
#' @param scaling The expected proportional increase in the number of cases above `baseline`
#'   in the event of an outbreak. If `baseline` is the number of expected cases under normal
#'   conditions, then `scaling * baseline` is the number of expected cases in the event of an
#'   outbreak. `scaling` can either be a scalar value or a column in `spacetime_data`.
#'
#' @inherit parallel_cusum_poisson details
#' @inherit parallel_cusum_gaussian2 return
#' @export
#' @md
#'
#' @seealso parallel_cusum_poisson
#' @family parallel2_alarms
#' @examples
#' data(NM_data)
#'
#' parallel_cusum_poisson2(NM_data, count, baseline_est)
#' parallel_cusum_poisson2(NM_data, count,
#'                         population * mean(count) / mean(population), scaling = 2)
parallel_cusum_poisson2 <- function(spacetime_data, outcome_col, baseline_col,
                                    scaling = 1.5) {

  params <- dplyr::transmute(spacetime_data,
                             id_space = .data$id_space,
                             id_time = .data$id_time,
                             .scaling = {{scaling}})
  wide_scaling <- pivot_for_scan(params, .data$.scaling)


  wide_baseline <- pivot_for_scan(spacetime_data,
                                  {{baseline_col}})
  wide_cases <- pivot_for_scan(spacetime_data,
                               {{outcome_col}})

  alarm_res <- parallel_cusum_poisson(wide_cases,
                                      wide_baseline,
                                      scaling = wide_scaling)

  alarm_res_df <- unpivot_parallel_alarms(alarm_res)
  dplyr::left_join(spacetime_data, alarm_res_df, by = c("id_space", "id_time"))
}

#' Calculate the Gaussian Shewhart alarm statistic over multiple spatial regions
#'
#' `parallel_shewhart_gaussian` calculates the value of the Shewhart alarm statistic assuming a
#' Gaussian distribution independently over each spatial region, over all time points. Each spatial
#' region and time point is considered independently.
#'
#' @inheritParams spacetime_data
#' @inheritParams parallel_cusum_gaussian2
#' @return
#' @export
#' @md
#'
#' @details The Shewhart alarm statistic is equal to the number of standard deviations that a given
#'  observation is above or below the mean, also known as a z-score. It can be directly calculated
#'  as `(cases - baseline - mean) / sigma`.
#'
#' @return `spacetime_data` with the added column `.action_level` containing the value of the
#'   Shewhart statistic at that point and time.
#' @family parallel2_alarms
#' @seealso parallel_shewhart_gaussian
#' @examples
#' data(NM_data)
#'
#' parallel_shewhart_gaussian2(NM_data, count)
#' parallel_shewhart_gaussian2(NM_data, count, baseline_est)
#' parallel_shewhart_gaussian2(NM_data, count,
#'                             population * mean(count) / mean(population),
#'                             sigma = 2)
parallel_shewhart_gaussian2 <- function(spacetime_data, outcome_col, baseline_col = NULL,
                                        mean = 0,
                                        sigma = 1) {
  validate_spacetime_data(spacetime_data)
  params <- dplyr::transmute(spacetime_data,
                             id_space = .data$id_space,
                             id_time = .data$id_time,
                             .mean = {{mean}},
                             .sigma = {{sigma}})
  wide_mean <- pivot_for_scan(params, .data$.mean)
  wide_sigma <- pivot_for_scan(params, .data$.sigma)


  wide_baseline <- pivot_for_scan(spacetime_data,
                                  {{baseline_col}})
  wide_cases <- pivot_for_scan(spacetime_data,
                               {{outcome_col}})


  alarm_res <- parallel_shewhart_gaussian(wide_cases,
                                          wide_baseline,
                                          mean = wide_mean,
                                          sigma = wide_sigma)

  alarm_res_df <- unpivot_parallel_alarms(alarm_res)
  dplyr::left_join(spacetime_data, alarm_res_df, by = c("id_space", "id_time"))

}
