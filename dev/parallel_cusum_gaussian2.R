parallel_cusum_gaussian2 <- function(data, outcome_name, baseline_name,
                                    mean = 0,
                                    sigma = 1,
                                    drift = 0.5) {

  wide_baseline <- pivot_for_scan(data,
                                  baseline_name)
  wide_cases <- pivot_for_scan(data,
                               outcome_name)


  alarm_res <- parallel_cusum_gaussian(wide_cases,
                                       wide_baseline,
                                       mean = 0,
                                       sigma = 1,
                                       drift = 0.5)

  alarm_res_df <- unpivot_parallel_alarms(alarm_res)
  left_join(data, alarm_res_df, by = c("id_space", "id_time"))
}
