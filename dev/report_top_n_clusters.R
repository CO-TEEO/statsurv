report_top_n_clusters <- function(scnsttst, top_n = 5, allow_overlapping_clusters) {
  # I want zone_number, locations, duration, score, relative_risk, and action_level
  # That's a bigger project than I can do right now.
  scnsttst$observed %>%
    dplyr::slice_max(score, n = top_n, with_ties = FALSE)
}
