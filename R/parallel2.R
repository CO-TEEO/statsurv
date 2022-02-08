parallel_cusum_gaussian2 <- function(data, outcome_col, baseline_col,
                                     mean = 0,
                                     sigma = 1,
                                     drift = 0.5) {

  wide_baseline <- pivot_for_scan(data,
                                  {{baseline_col}})
  wide_cases <- pivot_for_scan(data,
                               {{outcome_col}})


  alarm_res <- parallel_cusum_gaussian(wide_cases,
                                       wide_baseline,
                                       mean = mean,
                                       sigma = sigma,
                                       drift = drift)

  alarm_res_df <- unpivot_parallel_alarms(alarm_res)
  dplyr::left_join(data, alarm_res_df, by = c("id_space", "id_time"))
}

parallel_cusum_poisson2 <- function(data, outcome_col, baseline_col,
                                    scaling = 1.5) {
  wide_baseline <- pivot_for_scan(data,
                                  {{baseline_col}})
  wide_cases <- pivot_for_scan(data,
                               {{outcome_col}})

  alarm_res <- parallel_cusum_poisson(wide_cases,
                                      wide_baseline,
                                      scaling = scaling)

  alarm_res_df <- unpivot_parallel_alarms(alarm_res)
  dplyr::left_join(data, alarm_res_df, by = c("id_space", "id_time"))
}

parallel_shewhart_gaussian2 <- function(data, outcome_col, baseline_col,
                                        mean = 0,
                                        sigma = 1) {

  wide_baseline <- pivot_for_scan(data,
                                  {{baseline_col}})
  wide_cases <- pivot_for_scan(data,
                               {{outcome_col}})


  alarm_res <- parallel_shewhart_gaussian(wide_cases,
                                          wide_baseline,
                                          mean = mean,
                                          sigma = sigma)

  alarm_res_df <- unpivot_parallel_alarms(alarm_res)
  dplyr::left_join(data, alarm_res_df, by = c("id_space", "id_time"))

}
