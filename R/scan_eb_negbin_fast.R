#' @title Calculate the expectation-based negative binomial scan statistic via linear algebra
#'
#' @description  Calculate the expectation-based scan statistic using the a negative binomial
#'   distribution, as devised by Tango et al. (2011). Note that although this implemenation is based
#'   on that given in the \code{\link[scanstatistics:scan_eb_negbin]{scanstatistics}} package, it
#'   gives different results. In addition, the implementation here is typically 10-40x faster than
#'   the original implementation.
#'
#' @param wide_cases A matrix of observed counts. Rows indicate time and are ordered from least
#'   recent (row 1) to most recent (row `nrow(counts)`). Columns indicate locations, numbered from 1
#'   and up. Can be generated from a data.frame with the \code{\link{pivot_for_scan}} function.
#' @param key_matrix A matrix specifying how which locations belong to each zone. Each row is a
#'   different zone and each column is a location. An element `(i,j)` is 1 if location `j` is part
#'   of zone `i` and is 0 otherwise. Can be generated from a list specifying zones using the
#'   \code{\link{zones_to_key_matrix}} function.
#' @param wide_baseline A matrix of the same dimensions as `wide_cases`, holding the expected value
#'   of cases at each space-time location.This value is typically estimated from past data using a
#'   GLM or other model.
#' @inheritParams scanstatistics::scan_eb_negbin

#'
#' @inherit scanstatistics::scan_eb_negbin return references
#'
#' @details If \eqn{n} is the number of cases, \eqn{\mu} the expected number, and \eqn{\omega} the
#'   overispersion, then the score for each zone is \deqn{S = (n-\mu)/\omega * 1/\sqrt(\mu/\omega)}
#'   The scores reported by \code{\link[scanstatistics]{scan_eb_negbin}} do not include
#'   the square root in the denominator.
#'
#' @seealso \code{\link[scanstatistics]{scan_eb_negbin}}, \code{\link{pivot_for_scan}},
#'   \code{\link{zones_to_key_matrix}}
#' @export
#' @md
#' @examples
#' #Apply scan_eb_negbin_fast to a 3x3 spatial grid with 4 time points
#' #And inject an outbreak into the upper-left corner in the 2 most recent time points
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
#' scanres <- scan_eb_negbin_fast(wide_cases, key_matrix, wide_baseline,
#'                                n_mcsim = 10, thetas = 2)
scan_eb_negbin_fast <- function(wide_cases, key_matrix, wide_baseline,
                                thetas = 1,
                                n_mcsim = 0) {
  ### Helper functions #########################################
  calculate_ebnb_scores <- function(n_z, mu_z) {
    ebnb_score <- (n_z - mu_z) / sqrt(mu_z)
    ebnb_score[n_z < mu_z] <- 0
    return(ebnb_score)
  }
  calculate_ebnb_max <- function(n_z, mu_z) {
    m <- n_z > mu_z
    score <- suppressMessages(max(0, (n_z[m] - mu_z[m]) / sqrt(mu_z[m])))
    return(score)
  }

  simulate_nb_cases <- function(mu, theta, wide_cases) {
    n_locations <- ncol(wide_cases)
    cases <- stats::rnbinom(n = length(mu),
                            mu = mu,
                            size = theta)
    sim_cases <- matrix(cases, ncol = n_locations)
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
  if (is.matrix(thetas)) {
    wide_thetas <- flip_df(thetas)
  } else if (is_scalar_numeric(thetas)) {
    wide_thetas <- matrix(thetas, nrow = nrow(wide_cases), ncol = ncol(wide_cases))
  } else {
    stop("thetas must either be a matrix with the same dimensions as wide_cases or a scalar")
  }
  wide_omega <- 1 + wide_baseline / wide_thetas

  wide_n_omega <- wide_cases / wide_omega
  wide_mu_omega <- wide_baseline / wide_omega

  # Make the important matrices
  cum_n_omega <- simple_cumsum(wide_n_omega)
  cum_mu_omega <- simple_cumsum(wide_mu_omega)
  n_z <- key_matrix %*% t(cum_n_omega)
  mu_z <- key_matrix %*% t(cum_mu_omega)

  # Then calculate the scores
  ebnb_scores <- calculate_ebnb_scores(n_z, mu_z)

  # Then perform replicates
  mu <- unlist(wide_baseline)
  simulated_mlc_ebnb <- rep(NA, n_mcsim)

  for (ind in seq_len(n_mcsim)) {
    simulated_n <- simulate_nb_cases(mu, thetas, wide_cases)
    simulated <- simulated_n / wide_omega
    cum_simulated <- t(simple_cumsum(simulated))
    n_z <- key_matrix %*% cum_simulated
    simulated_mlc_ebnb[[ind]] <- calculate_ebnb_max(n_z, mu_z)
  }



  #Then we have to do some extra work to get these into the form we expect.
  zones_vector <- as.vector(zones_matrix)
  durations_vector <- as.vector(durations_matrix)
  score <- as.vector(ebnb_scores)
  observed <- data.frame("zone" = zones_vector,
                         "duration" = durations_vector,
                         "score" = score) %>%
    dplyr::arrange(dplyr::desc(.data$score))
  ind <- which.max(observed$score)
  mlc_row <- observed[ind, , drop = FALSE]
  MLC <- list("zone_number" = mlc_row$zone,
              "locations" = which(key_matrix[mlc_row$zone, , drop = TRUE] == 1),
              "duration" = mlc_row$duration,
              "score" = mlc_row$score,
              "relative_risk" = mlc_row$relrisk)
  n <- length(simulated_mlc_ebnb)
  replicates <- data.frame(zone = rep(NA, n),
                           duration = rep(NA, n),
                           score = simulated_mlc_ebnb)
  MC_pvalue <- mc_pvalue(MLC$score, replicates$score)
  Gumbel_pvalue <- NULL
  xres_ebnb <- structure(
    list(distribution = "negative binomial",
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

  return(xres_ebnb)
}
