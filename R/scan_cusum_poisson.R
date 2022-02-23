#' @title Calculate the CUSUM statistic over a set of spatial-temporal zones
#'
#' @description Calculate the CUSUM alarm statistic over a large set of spatio-temporal zones, as
#'   described by Sonesson (2007).
#'
#' @param scaling A scalar number > 1. Represents the proportional increase in the number of cases
#'   beyond `wide_baseline` in the event of an outbreak. If `wide_baseline` is the number of
#'   expected cases under normal conditions, then `scaling * wide_baseline` is the number of
#'   expected cases in the event of an outbreak. Used to calculate the downward drift.
#' @inheritParams scan_eb_negbin_fast
#'
#' @details The CUSUM statistic is widely used surveillance technique for purely temporal processes.
#'   It is calculated by taking the cumulative sum of the standard residuals with a downward drift
#'   and a floor of 0. For a Poisson process \eqn{x(t)} with expected value \eqn{\lambda} and an
#'   expected value during an ourbreak of \eqn{\delta\lambda} the optimal downward drift is \eqn{k =
#'   \lambda(\delta - 1) / (log(\delta)}.
#'
#' When applied as a scan statistic, the total number of cases and the expected number of cases are
#' aggregated for a large number of spatial-temporal zones, where each zone consists of one or more
#' neighboring spatial locations.
#' For each zone, a single CUSUM statistic is calculated, incorporating information from the
#' earliest time point to the most recent. In the returned value, all `duration` fields refer to
#' the number of time points since the CUSUM statistic was last equal to 0. This is different from
#' the behavoir of the other scan-type functions, where the duration indicates the size of the
#' temporal window.
#'
#' @inherit scan_eb_poisson_fast return
#' @references
#'   Sonesson, C. (2007). \emph{A CUSUM framework for detection of space-time disease clusters using
#'   scan statistics}, Stat Med 26(26): 4770-4789.
#' @seealso \code{\link{parallel_cusum_poisson}}, \code{\link{pivot_for_scan}},
#'   \code{\link{zones_to_key_matrix}}
#' @export
#' @md
#' @examples
#' # Apply scan_cusum_poisson to a 3x3 spatial grid with 4 time points
#' # And inject an outbreak into the upper-left corner in the 2 most recent time points
#' library("magrittr")
#' x_coord <- rep(c(1:3), 3)
#' y_coord <- rep(c(1:3), each =3)
#' geo <- matrix(c(x_coord, y_coord), ncol = 2, byrow = FALSE)
#' zones <- geo %>%
#'   scanstatistics::coords_to_knn(k = 4) %>%
#'   scanstatistics::knn_zones()
#' key_matrix <- zones_to_key_matrix(zones)
#' outbreak_sp <- c(1, 2, 4, 5)
#' outbreak_tm <- c(3, 4)
#' wide_cases <- matrix(2, nrow = 4, ncol = 9)
#' wide_cases[outbreak_tm, outbreak_sp] <- 5
#' wide_cases[c(3, 4), c(8, 9)] <- 2.5

#' wide_baseline <- matrix(2, nrow = 4, ncol = 9)
#' scanres <- scan_cusum_poisson(wide_cases, key_matrix, wide_baseline,
#'                               scaling = 1.5,
#'                               n_mcsim = 10)

scan_cusum_poisson <- function(wide_cases,
                               key_matrix,
                               wide_baseline,
                               scaling = 1.5,
                               n_mcsim = 100) {


    single_scan <- function(curr_wide_cases,
                            key_matrix,
                            drift_zone_list,
                            baseline_zone_list,
                            only_mlc = FALSE,
                            zones) {
        # The function that actually calcultes the CUSUM scores.
        count_zone_mat <- key_matrix %*% t(curr_wide_cases)
        colnames(count_zone_mat) <- rownames(count_zone_mat)
        count_zone_list <- mat_to_list(count_zone_mat)

        cusum_zone_list <- lapply(count_zone_list, function(x) x * NA)
        prev_cusum <- count_zone_list[[1]] * 0 # We start with a CUSUM score of zero
        durations <- rep(0, nrow(key_matrix)) # And a duration of zero
        numer_relrisk <- rep(0, nrow(key_matrix))
        denom_relrisk <- rep(0, nrow(key_matrix))
        for (dateInd in seq_len(length(count_zone_list))) {
            cusum_zone_list[[dateInd]] <- prev_cusum + count_zone_list[[dateInd]] -
                drift_zone_list[[dateInd]]
            # CUSUM has a floor at zero we need to implement.
            m <- cusum_zone_list[[dateInd]] <= 0
            cusum_zone_list[[dateInd]][m] <- 0
            if (!only_mlc) {
                # Calculations for the duration and relative risk
                durations[!m] <- durations[!m] + 1
                durations[m] <- 0
                numer_relrisk <- ifelse(!m, numer_relrisk + count_zone_list[[dateInd]], 0)
                denom_relrisk <- ifelse(!m, denom_relrisk + baseline_zone_list[[dateInd]], 0)
            }
            prev_cusum <- cusum_zone_list[[dateInd]]
        }

        # Get the results in a convenient form
        zone_labels <- seq_len(length(count_zone_list[[1]]))
        score_zones <- prev_cusum
        if (only_mlc) {
            return(max(score_zones))
        } else {
            mlc_i <- which.max(score_zones)
            MLC_out <- list(zone_number = zone_labels[mlc_i],
                            locations = zones[[zone_labels[mlc_i]]],
                            duration = durations[mlc_i],
                            score = score_zones[mlc_i],
                            relative_risk = numer_relrisk[mlc_i] / denom_relrisk[mlc_i])


            relative_risk <- numer_relrisk / denom_relrisk
            observed <- data.frame("zone" = zone_labels,
                                   "duration" = durations,
                                   "score" = score_zones,
                                   "relrisk" = relative_risk) %>%
                dplyr::arrange(dplyr::desc(.data$score))
            return(list("observed" = observed, "MLC_out" = MLC_out))
        }
    }
    ### Argument Checks #########################################
    if (is.list(key_matrix)) {
        key_matrix <- zones_to_key_matrix(key_matrix)
    }

    ### The actual computation #########################
    incontrol_zone_mat <- key_matrix %*% t(wide_baseline)
    colnames(incontrol_zone_mat) <- rownames(wide_baseline)


    outcontrol_zone_mat <- incontrol_zone_mat * scaling


    drift_numer <- outcontrol_zone_mat - incontrol_zone_mat
    drift_denom <- log(outcontrol_zone_mat / incontrol_zone_mat)
    drift_zone_mat <- drift_numer / drift_denom # Formula for the drift is from Lucas et al., 1985

    incontrol_zone_list <- mat_to_list(incontrol_zone_mat) # Convert matrices to lists for speed
    drift_zone_list <- mat_to_list(drift_zone_mat)

    zones <- key_matrix_to_zones(key_matrix)
    scan_res <- single_scan(wide_cases,
                            key_matrix,
                            drift_zone_list,
                            incontrol_zone_list,
                            only_mlc = FALSE,
                            zones)
    observed <- scan_res$observed
    MLC_out <- scan_res$MLC_out

    # Run the MC replicates
    simulated_scores <- rep(0, n_mcsim)
    mu_rep <- as.vector((wide_baseline))
    for (ind in seq_len(n_mcsim)) {
        sim_rep <- stats::rpois(length(mu_rep), mu_rep)
        sim_cases <- matrix(sim_rep, ncol = ncol(wide_baseline))
        simulated_scores[[ind]] <- single_scan(sim_cases,
                                               key_matrix,
                                               drift_zone_list,
                                               incontrol_zone_list,
                                               only_mlc = TRUE)
    }

    # Get the data into the scanstatistics class
    mc_pvalue <- mc_pvalue(MLC_out$score, simulated_scores)
    n <- length(simulated_scores)
    replicates <- data.frame(zone = rep(NA, n),
                             duration = rep(NA, n),
                             relrisk = rep(NA, n),
                             score = simulated_scores)
    xres <- structure(
        list(distribution = "Poisson",
             type = "cusum-based",
             setting = "univariate",
             MLC = MLC_out,
             observed = observed,
             replicates = replicates,
             MC_pvalue = mc_pvalue,
             Gumbel_pvalue = NULL,
             n_zones = length(zones),
             n_locations = ncol(wide_cases),
             max_duration = nrow(wide_cases),
             n_mcsim = n_mcsim),
        class = "scanstatistic")
    return(xres)
}
