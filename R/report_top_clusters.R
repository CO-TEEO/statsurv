#' @title Extract clusters identified by scanning alarm functions
#'
#' @description Extract all space-time clusters, based on values calculated by a scan statistic.
#'   This can either be an absolute measure of intensity, such as the `score` calculate by
#'   \code{\link[scanstatistics]{scan_eb_poisson}} or a relative measure, such as the Monte-Carlo
#'   p-value.
#'
#' @details `scanstatistics` objects contain detailed information about the likelihood of a cluster
#'   in all possible spatial zones and temporal durations. `report_top_clusters` uses this
#'   information to report the zones most likely to contain a cluster. The results can be customized
#'   using the `cutoff_val`, `max_reported`, `min_reported`, and `single_duration` options.
#'
#' @inheritParams remove_overlapping_clusters
#' @param cutoff_val The cutoff value used for reporting clusters. If `take_highest = TRUE`, only
#'   clusters with values of `order_by` greater than `cutoff_val` will be reported; if
#'   `take_highest = FALSE`, only report clusters with values under `cutoff_val`.
#'   Defaults to `NULL`, where no cutoff value is applied.
#' @param max_reported The maximum number of clusters to be reported. Defaults to 10.
#' @param min_reported The minimum number of clusters to be reported. This number of clusters will
#'   always be reported, even if they do not meet the threshold set by `cutoff_val`.
#' @param single_duration Boolean. If TRUE, remove clusters with the same spatial zone but different
#'   duration as another, more intense, cluster.
#'
#' @return A data frame containing information about the most likely clusters in the study area,
#'   with between `min_reported` and `max_reported` rows, and including columns `zone`, `duration`,
#'   and the column specified by `order_by`.
#'
#' @seealso remove_overlapping_clusters
#' @export
#' @md
#' @examples
#' library(sf)
#' data("NM_data")
#' data("NM_county_sf")
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' scanres <- scan_eb_poisson2(NM_data, count, zones,
#'                             baseline_est, n_mcsim = 100)
#'
#' report_top_clusters(scanres, score)
#'
#' report_top_clusters(scanres, mc_pvalue,
#'                     take_highest = FALSE,
#'                     cutoff_val = 0.05)
#'
#' # Always report at least 3 clusters:
#' report_top_clusters(scanres, mc_pvalue,
#'                     take_highest = FALSE,
#'                     cutoff_val = 0.05,
#'                     min_reported = 3)
#'
#' # First remove overlapping clusters, then report:
#' remove_overlapping_clusters(scanres, zones,
#'                             order_by = mc_pvalue,
#'                             take_highest = FALSE,
#'                             max_overlap_frac = 0.2) %>%
#'   report_top_clusters(mc_pvalue, take_highest = FALSE,
#'                       cutoff_val = 0.05)
report_top_clusters <- function(scan_result,
                                order_by,
                                take_highest = TRUE,
                                cutoff_val = NULL,
                                max_reported = 10,
                                min_reported = 1,
                                single_duration = TRUE) {

  ### Argument Checks ----
  stopifnot(inherits(scan_result, "scanstatistic"),
            is_scalar_numeric(max_reported))

  ### Generating the data frame we actually use ----
  if (max_reported <= 0) {
    stop("max_reported must be an integer greater than or equal to 1")
  }
  if (is.null(cutoff_val)) {
    cutoff_val <- Inf * (-1)^take_highest #Inf if take_highest, -1 if not
  }

  if (scan_result$type == "Bayesian") {
    observed_alarms <- scan_result$posteriors$window_posteriors
  } else {
    observed_alarms <- scan_result$observed
  }
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

  return(observed_alarms)

}



#' Remove overlapping clusters from scan statistics
#'
#'
#' @description  In addition to reporting the details of the most likely cluster (MLC),
#'   `scanstatistics` objects also contain detailed information about the likelihood of a cluster in
#'   all possible spatial zones and temporal durations. This information can be used to identify any
#'   independent secondary clusters that exist in the study area. When searching for these secondary
#'   clusters, we are generally interested in clusters that do not significantly overlap the primary
#'   cluster.
#'
#' @details  `remove_overlapping_clusters` goes through the information about the likelihood of a
#'   cluster in all possible spatial zones and temporal durations, and only keeps the information
#'   from non-overlapping spatial zones. Potential clusters are gone through from most-likely to
#'   least likely (as determined by `order_by` and `take_highest`). Information about a spatial zone
#'   and duration is retained if the fraction of spatial locations in the zone that are present in
#'   any already identified cluster is less than or equal to `max_overlap_frac`.
#'
#'
#' @param scan_result An object of class `scanstatistic`, such as generated by
#'   \code{\link[scanstatistics]{scan_bayes_negbin}} or \code{\link{scan_eb_poisson2}}.
#' @param order_by The column to order the clusters by. Any column in `scan_result$observed` or
#'   `scan_result$posteriors$window_posteriors` is valid. Specified as bare text.
#' @param take_highest Boolean. Should the clusters by ordered from highest to lowest (`take_highest
#'   = TRUE`) or from lowest to highest (`take_highest = FALSE`).
#' @param max_overlap_frac Optional. What is the maximum overlap fraction that should be allowed
#'   between clusters? Defaults to 0.
#' @inheritParams scanstatistics::scan_eb_negbin
#'
#' @return An object of class `scanstatistic`, with the `observed` or `posteriors$window_posteriors`
#'   data frame modified to only contain non-overlapping clusters. Otherwise identical to the
#'   `scan_result` input.
#' @export
#' @md
#' @seealso report_top_clusters
#'
#' @examples
#' library(sf)
#' data("NM_data")
#' data("NM_county_sf")
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' scanres <- scan_eb_poisson2(NM_data, count, zones,
#'                             baseline_est, n_mcsim = 100)
#'
#' # Order by cluster score
#' remove_overlapping_clusters(scanres, zones, order_by = score)
#'
#' # Order by mc-pvalue
#' remove_overlapping_clusters(scanres, zones, order_by = mc_pvalue,
#'                             take_highest = FALSE)
#'
#' # Order by mc-pvalue, allow some overlap
#' remove_overlapping_clusters(scanres, zones, order_by = mc_pvalue,
#'                             take_highest = FALSE,
#'                             max_overlap_frac = 0.2)
remove_overlapping_clusters <- function(scan_result, zones, order_by,
                                        take_highest = TRUE, max_overlap_frac = 0) {

  # For a list of zone_inds, returns the ones that have no points in common
  # zone_inds are gone through in order, front to back.
  if (scan_result$type == "Bayesian") {
    observed_alarms <- scan_result$posteriors$window_posteriors
  } else {
    observed_alarms <- scan_result$observed
  }
  all_tracts <- sort(unique(unlist(zones)))
  if (take_highest) {
    observed_alarms <- dplyr::arrange(observed_alarms, dplyr::desc({{order_by}}))
  } else {
    observed_alarms <- dplyr::arrange(observed_alarms, {{order_by}})
  }
  observed_zones <- observed_alarms$zone

  ii <- 0
  good_rows <- rep(FALSE, nrow(observed_alarms))
  for (curr_zone in observed_zones) { #Loop through the zones
    ii <- ii + 1
    mask <- zones[[curr_zone]] %in% all_tracts
    overlap_fraction <- mean(!mask)
    if (overlap_fraction <= max_overlap_frac) {
      # If we find one with no tracts that have already been in a cluster
      good_rows[[ii]] <- TRUE
      inverse_mask <- !(all_tracts %in% zones[[curr_zone]])
      all_tracts <- all_tracts[inverse_mask]
    }
  }
  if (scan_result$type == "Bayesian") {
    scan_result$posteriors$window_posteriors <- observed_alarms[good_rows, , drop = FALSE]
  } else {
    scan_result$observed <- observed_alarms[good_rows, , drop = FALSE]
  }
  scan_result
}


