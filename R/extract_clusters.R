#' @title Extract clusters identified by scanning alarm functions
#'
#' @description Extract all space-time clusters, based on values calculated by a scan statistic.
#'   This can either be an absolute measure of intensity, such as the score calculate by
#'   \code{\link[scanstatistics]{scan_eb_poisson}} or a relative measure, such as the Monte-Carlo
#'   p-value.
#'
#' @details
#' A cluster is defined as a spatio-temporal zone with a calculated value above or below a specified
#' cutoff score. One cluster is always returned, even if its value does not meet the cutoff
#' requirements, and at most `max_allowed` clusters are reported. A cluster is never reported if it
#' covers exactly the same spatial zone but has a different duration as another more intense
#' cluster. If `allow_overlap` is FALSE, clusters are only reported if they have no spatial regions
#' in common with a more intense cluster.
#'
#' @param scan_result The result from a scan-type alarm function
#'   (\code{\link{standardized_alarm_functions}}).
#' @param zones A list of integer vectors. Each vector corresponds to a single zone; its elements
#'   are the numbers of the locations in that zone. Must be generated as part of calculating the
#'   scan statistic.
#' @param use_column How should clusters be identified? Allowed options are "action_level",
#'   "pvalue_mc", "score", and "log_posterior". "action_level" is valid for all scanstatistic
#'   objects returned by \code{\link{standardized_alarm_functions}}, "pvalue_mc" and "score" are
#'   valid for all scanstatistics except for \code{\link[scanstatistics]{scan_bayes_negbin}}, and
#'   "log_posterior" is valid only for \code{\link[scanstatistics]{scan_bayes_negbin}}.
#' @param cutoff The minimun or maximum value of `use_column` to be defined as a cluster. Values
#'   must be below `cutoff` if `use_column = "pvalue_mc"`; otherwise values must be above `cutoff`.
#' @param max_allowed The maximum number of clusters to be reported.
#' @param allow_overlap Boolean. If FALSE, clusters are only reported if they have no spatial
#'   regions in common with a more intense cluster.
#'
#' @return A `data.frame` with at most `max_allowed` rows, and including columns `zone`, `duration`,
#'   and whichever column was specified by `use_column`.
#' @export
#' @md
#' @examples
#' # Using pre-computer alarm statistics for New Mexico
#' alarm <- statsurv::nm_scan_alarms[["1989"]]
#'
#' # By default, use the scan statistics score as the ranking mechanism:
#' extract_clusters(alarm,
#'                  alarm$zone_info)
#'
#' # Use a cutoff of a p-value of < 0.05:
#' extract_clusters(alarm,
#'                  alarm$zone_info,
#'                  use_column = "mc_pvalue")
extract_clusters <- function(scan_result,
                             zones,
                             use_column = c("action_level", "mc_pvalue", "score", "log_posterior"),
                             cutoff = 0.1,
                             max_allowed = length(zones),
                             allow_overlap = TRUE) {

  ### Argument Checks ----
  check_type(scan_result, "scanstatistic") #Should I allow bare data frames?
  check_type(zones, "list")
  use_column <- match.arg(use_column)
  check_scalar_type(cutoff, "numeric")
  check_scalar_type(max_allowed, "numeric")
  check_scalar_type(allow_overlap, "logical")

  ### Generating the data frame we actually use ----
  if (max_allowed > length(zones) || max_allowed <= 0) {
    stop("max_allowed must be between 1 and the number of zones")
  }

  if (use_column == "log_posterior") {
    obs_clusters <- scan_result$posterior$window_posteriors
  } else {
    obs_clusters <- scan_result$observed
  }

  if (use_column == "mc_pvalue") {
    if (scan_result$type == "Bayesian") {
      stop("use_column = 'mc_pvalue' cannot be used with Bayesian scan statistics.",
           "use 'action_level' or 'log_posterior' instead")
    }
    if (nrow(scan_result$replicates) == 0) {
      stop("No replicates - use_column = 'mc_pvalue' is invalid. ",
      "Use a different value for 'use_column' or re-run the scan statistic ",
      "with a different value of n_mcsim")
    }
    if ("action_level" %in% names(obs_clusters)) {
      obs_clusters$mc_pvalue <- mc_pvalue(obs_clusters$action_level,
                                          scan_result$replicates$action_level)
    } else {
      obs_clusters$mc_pvalue <- mc_pvalue(obs_clusters$score,
                                          scan_result$replicates$score)
    }
  }

  obs_clusters$.val_to_use <- obs_clusters[[use_column]]

  ### Select out clusters ----
  if (use_column == "mc_pvalue") {
    obs_clusters <- obs_clusters %>%
      dplyr::arrange(.data$.val_to_use)
    mask <- obs_clusters$.val_to_use <= cutoff
  } else {
    obs_clusters <- obs_clusters %>%
        dplyr::arrange(dplyr::desc(.data$.val_to_use))
    mask <- obs_clusters$.val_to_use >= cutoff
  }
  mask[1] <- TRUE
  good_clusters <- obs_clusters[mask, , drop = FALSE]



  if (!allow_overlap) {
    zone_inds <- good_clusters$zone
    mask <- zone_inds %in% check_overlap(zone_inds, zones)
    good_clusters <- good_clusters[mask, , drop = FALSE]
  }

  good_clusters <- good_clusters %>%
    dplyr::group_by(.data$zone) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup()

  if (nrow(good_clusters) > max_allowed) {
    good_clusters <- good_clusters[seq_len(max_allowed), ]
  }

  good_clusters$.val_to_use <- NULL
  return(good_clusters)

}

check_overlap <- function(zone_inds, zones) {
  # For a list of zone_inds, returns the ones that have no points in common
  # zone_inds are gone through in order, front to back.
  all_tracts <- unique(unlist(zones[zone_inds]))
  i <- 0
  good_zones <- list()
  for (zInd in zone_inds) {
    mask <- zones[[zInd]] %in% all_tracts
    if (all(mask)) {
      i <- i + 1
      good_zones[[i]] <- zInd
      inverse_mask <- !(all_tracts %in% zones[[zInd]])
      all_tracts <- all_tracts[inverse_mask]
    }
  }

  good_zones <- as.numeric(good_zones)
  return(good_zones)
}


