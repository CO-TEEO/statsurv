#' @title Calculate the expectation-based Poisson scan statistic via linear algebra
#'
#' @description  Calculate the expectation-based scan statistic using the a Poisson distribution, as
#'   devised by Neill (2009). This function should give identical results to those calculated by
#'   \code{\link[scanstatistics]{scan_eb_poisson}}, but is significantly faster when scanning large
#'   areas.
#'
#' @inheritParams scan_eb_negbin_fast
#'
#' @inherit scanstatistics::scan_eb_poisson return references
#' @export
#' @md
#' @seealso \code{\link[scanstatistics]{scan_eb_poisson}}, \code{\link{scan_eb_negbin_fast}},
#' \code{\link{pivot_for_scan}}, \code{\link{zones_to_key_matrix}}
#' @examples
#' # Apply scan_eb_poisson_fast to a 3x3 spatial grid with 4 time points
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
#' scanres <- scan_eb_poisson_fast(wide_cases, key_matrix, wide_baseline,
#'                                 n_mcsim = 10)

scan_eb_poisson_fast <- function(wide_cases, key_matrix, wide_baseline, n_mcsim) {

    ### Helper functions #########################################
    calculate_ebp_scores <- function(n_z, mu_z) {
        ebp_poisson_score <- n_z * log(n_z / mu_z) + mu_z - n_z
        ebp_poisson_score[n_z < mu_z] <- 0
        return(ebp_poisson_score)
    }
    calculate_ebp_max <- function(n_z, mu_z) {
        m <- n_z > mu_z
        score <- max(0, n_z[m] * log(n_z[m] / mu_z[m]) + mu_z[m] - n_z[m])
        return(score)
    }

    flip_df <- function(df) {
        df[seq(nrow(df), 1, by = -1), , drop = FALSE]
    }

    simple_cumsum <- function(wide_x) {
        if (is.vector(wide_x)) {
            return(matrix(wide_x[1, ], nrow = 1))
        }
        if (nrow(wide_x) == 1) {
            return(wide_x)
        }
        return(apply(wide_x, 2, cumsum))
    }

    simulate_cases <- function(lambda, wide_cases) {
        n_locations <- ncol(wide_cases)
        sim_cases <- matrix(stats::rpois(length(lambda), lambda), ncol = n_locations)
        return(sim_cases)
    }

    ### Argument Checks #########################################
    if (is.list(key_matrix)) {
        key_matrix <- zones_to_key_matrix(key_matrix)
    }

    ### Computations ##################################
    key_matrix <- Matrix::Matrix(key_matrix)
    n_zones <- nrow(key_matrix)
    n_locations <- ncol(key_matrix)
    n_durations <- nrow(wide_baseline)
    zones_matrix <- matrix(rep(seq_len(n_zones), n_durations), ncol = n_durations)
    durations_matrix <- matrix(rep(seq_len(n_durations), each = n_zones), ncol = n_durations)

    wide_cases <- flip_df(wide_cases)
    wide_baseline <- flip_df(wide_baseline)



    # Make the important matrices
    cum_observed <- simple_cumsum(wide_cases)
    cum_expected <- simple_cumsum(wide_baseline)
    n_z <- key_matrix %*% t(cum_observed)
    mu_z <- key_matrix %*% t(cum_expected)

    # Then calculate the scores
    ebp_scores <- calculate_ebp_scores(n_z, mu_z)
    relative_risk <- n_z / mu_z

    # Then perform replicates
    lambda <- unlist(wide_baseline)
    simulated_mlc_ebp <- rep(NA, n_mcsim)

    for (ind in seq_len(n_mcsim)) {
        simulated <- simulate_cases(lambda, wide_cases)
        cum_simulated <- t(simple_cumsum(simulated))
        n_z <- key_matrix %*% cum_simulated
        simulated_mlc_ebp[[ind]] <- calculate_ebp_max(n_z, mu_z)
    }



    #Then we have to do some extra work to get these into the form we expect.
    zones_vector <- as.vector(zones_matrix)
    durations_vector <- as.vector(durations_matrix)
    risk_vector <- as.vector(relative_risk)
    score <- as.vector(ebp_scores)
    observed <- data.frame("zone" = zones_vector,
                           "duration" = durations_vector,
                           "score" = score,
                           "relrisk" = risk_vector) %>%
        dplyr::arrange(dplyr::desc(score))
    ind <- which.max(observed$score)
    mlc_row <- observed[ind, , drop = FALSE]
    MLC <- list("zone_number" = mlc_row$zone,
                "locations" = which(key_matrix[mlc_row$zone, , drop = TRUE] == 1),
                "duration" = mlc_row$duration,
                "score" = mlc_row$score,
                "relative_risk" = mlc_row$relrisk)
    n <- length(simulated_mlc_ebp)
    replicates <- data.frame(zone = rep(NA, n),
                             duration = rep(NA, n),
                             relrisk = rep(NA, n),
                             score = simulated_mlc_ebp)

    MC_pvalue <- mc_pvalue(MLC$score, replicates$score)
    Gumbel_pvalue <- NULL
    xres_ebp <- structure(
        list(distribution = "Poisson",
             type = "expectation-based",
             setting = "univariate",
             MLC = MLC,
             observed = observed,
             replicates = replicates,
             MC_pvalue = MC_pvalue,
             Gumbel_pvalue = Gumbel_pvalue,
             n_zones = n_zones,
             n_locations = n_locations,
             max_duration = n_durations,
             n_mcsim = n_mcsim),
        class = "scanstatistic")
    return(xres_ebp)
}
