scan_eb_poisson2 <- function(data, outcome_name, baseline_name,
                             zones, n_mcsim = 0, gumbel = FALSE, max_only = FALSE) {

  wide_baseline <- pivot_for_scan(data,
                                  baseline_name)
  wide_cases <- pivot_for_scan(data,
                               outcome_name)

  key_matrix <- zones_to_key_matrix(zones)

  scan_eb_poisson_fast(wide_cases, key_matrix, wide_baseline, n_mcsim = n_mcsim)
  # Then some code here to tidy up the result
}
