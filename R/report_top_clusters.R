
remove_overlapping_clusters <- function(scan_result, zones, order_by, take_highest = TRUE, max_overlap_frac = 0) {

  # For a list of zone_inds, returns the ones that have no points in common
  # zone_inds are gone through in order, front to back.
  observed_alarms <- scan_result$observed
  all_tracts <- sort(unique(unlist(zones)))
  if (take_highest) {
    observed_alarms <- dplyr::arrange(observed_alarms, dplyr::desc({{order_by}}))
  } else {
    observed_alarms <- dplyr::arrange(observed_alarms, {{order_by}})
  }
  observed_zones <- scan_result$observed$zone

  ii <- 0
  good_rows <- rep(FALSE, nrow(observed_alarms))
  for (curr_zone in observed_zones) { #Loop through the zones
    ii <- ii + 1
    mask <- zones[[curr_zone]] %in% all_tracts
    overlap_fraction = mean(!mask)
    if (overlap_fraction <= max_overlap_frac) { # If we find one with no tracts that have already been in a cluster
      good_rows[[ii]] <- TRUE
      inverse_mask <- !(all_tracts %in% zones[[curr_zone]])
      all_tracts <- all_tracts[inverse_mask]
    }
  }
  scan_result$observed <- observed_alarms[good_rows, , drop = FALSE]
  scan_result
}


report_top_clusters <- function(scan_result,
                                order_by,
                                take_highest = TRUE,
                                cutoff_val = NULL,
                                max_reported = 10,
                                min_reported = 1,
                                single_duration = TRUE) {

  ### Argument Checks ----
  check_type(scan_result, "scanstatistic") #Should I allow bare data frames?
  # check_scalar_type(cutoff_val, "numeric")
  check_scalar_type(max_reported, "numeric")

  ### Generating the data frame we actually use ----
  if (max_reported <= 0) {
    stop("max_reported must be an integer greater than or equal to 1")
  }
  if (is.null(cutoff_val)) {
    cutoff_val <- Inf * (-1)^take_highest #Inf if take_highest, -1 if not
  }

  observed_alarms <- scan_result$observed
  if (single_duration) {
    observed_alarms <- observed_alarms %>%
      dplyr::group_by(.data$zone) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
  }

  if (take_highest) {
    observed_alarms <- dplyr::arrange(observed_alarms, dplyr::desc({{order_by}}))
    observed_alarms <- observed_alarms %>%
      dplyr::filter({{order_by}} >= cutoff_val | dplyr::row_number() <= min_reported)
  } else {
    observed_alarms <- dplyr::arrange(observed_alarms, {{order_by}}) %>%
      dplyr::filter({{order_by}} <= cutoff_val | dplyr::row_number() <= min_reported)
  }



  observed_alarms <- observed_alarms %>%
    dplyr::slice_head(n = max_reported)

  # good_clusters$.val_to_use <- NULL
  return(observed_alarms)

}
