#' @title Calculate the Poisson CUSUM statistic over multiple spatial regions
#'
#' @description `parallel_cusum_poisson` calculates the Poisson CUSUM statistic independently over
#'   each spatial region, over all time points. Unlike \code{\link{scan_cusum_poisson}}, information
#'   from multiple spatial regions is never aggregated.
#'
#' @param scaling The expected proportional increase in the number of cases  above `wide_baseline`
#'   in the event of an outbreak. If `wide_baseline` is the number of expected cases under normal
#'   conditions, then `scaling * wide_baseline` is the number of expected cases in the event of an
#'   outbreak. `scaling` can be a scalar, a vector with a separate value for each spatial region, or
#'   a matrix the same dimensions as `wide_cases`
#' @inheritParams scan_cusum_poisson
#'
#' @details
#'   The CUSUM statistic is widely used surveillance technique for purely temporal processes.
#'   It is calculated by taking the cumulative sum of the standard residuals with a downward drift
#'   and a floor of 0. For a Poisson process \eqn{x(t)} with expected value \eqn{\lambda} and an
#'   expected value during an ourbreak of \eqn{\delta\lambda} the optimal downward drift is \eqn{k =
#'   \lambda(\delta - 1) / (log(\delta)}.
#'
#'
#' @return A matrix with the same dimensions as `wide_cases`, where rows indicate time (oldest to
#'   newest) and columns indicate locations. Element `(i, j) ` indicates the value of the CUSUM
#'   statistic at time `i` in region `j`. The value of the CUSUM statistic at row `i` incorporates
#'   information from time points `1` thought `i`.
#'
#' @seealso \code{\link{scan_cusum_poisson}}, \code{\link{pivot_for_scan}},
#'   \code{\link{parallel_cusum_gaussian}}
#' @export
#' @md
#' @examples
#' outbreak_sp <- c(1, 2, 4, 5)
#' outbreak_tm <- c(3, 4)
#' wide_cases <- matrix(2, nrow = 4, ncol = 9)
#' wide_cases[outbreak_tm, outbreak_sp] <- 5
#' wide_cases[c(3, 4), c(8, 9)] <- 2.5
#' wide_baseline <- matrix(2, nrow = 4, ncol = 9)
#' scanres <- parallel_cusum_poisson(wide_cases, wide_baseline,
#'                                   scaling = 1.5)
#'
#' v_scaling <- c(rep(1.5, 8), 3) # Special scaling for region 9
#' scanres <- parallel_cusum_poisson(wide_cases, wide_baseline,
#'                                   scaling = v_scaling)
#'
#'
#' m_scaling <- matrix(1.5, nrow = 4, ncol = 9)
#' m_scaling[4, 1] <- 3
#' scanres <- parallel_cusum_poisson(wide_cases, wide_baseline,
#'                                   scaling = m_scaling)
parallel_cusum_poisson <- function(wide_cases,
                                   wide_baseline,
                                   scaling = 1.5) {

  ### Argument checks ----
  stopifnot(is.matrix(wide_cases),
            is.matrix(wide_baseline),
            is.numeric(scaling))

  ### Computations ----
  t_wide_cases <- t(wide_cases)
  cases_list <- mat_to_list(t_wide_cases)
  cusum_list <- lapply(cases_list, function(x) x * NA)
  prev_cusum <- cases_list[[1]] * 0 # We start with a CUSUM score of zero

  incontrol <- wide_baseline
  scaling <- pad_cusum_inputs(scaling, wide_baseline)
  outcontrol <- incontrol * scaling
  drift_numer <- outcontrol - incontrol
  drift_denom <- log(outcontrol / incontrol)
  drift_mat <- drift_numer / drift_denom # Formula for the drift is from Lucas et al., 1985
  drift_list <- mat_to_list(t(drift_mat))

  for (date_ind in seq_len(length(cases_list))) {
    cusum_list[[date_ind]] <- pmax(prev_cusum + cases_list[[date_ind]] - drift_list[[date_ind]],
                                   0)
    prev_cusum <- cusum_list[[date_ind]]
  }

  cusum_df <- as.data.frame(cusum_list)
  wide_cusum_df <- t(cusum_df)
  colnames(wide_cusum_df) <- colnames(wide_cases)
  rownames(wide_cusum_df) <- rownames(wide_cases)
  return(as.matrix(wide_cusum_df))
}

#' @title Calculate the Gaussian CUSUM statistic over multiple spatial regions
#'
#' @description `parallel_cusum_gaussian` calculates the Gaussian CUSUM statistic independently over
#'   each spatial region, over all time points. Unlike e.g., \code{\link{scan_cusum_poisson}},
#'   information from multiple spatial regions is never aggregated.
#'
#' @param mean The estimated mean of the residuals (`wide_cases - wide_baseline`)
#' @param sigma The estimated standard deviation of the residuals.
#' @param drift The downward drift to use in calculating the CUSUM statistic.  The value of `drift`
#'   is typically half of the expected increase in the value of the standardized residuals during an
#'   outbreak.
#' @inheritParams parallel_cusum_poisson
#'
#' @details The CUSUM statistic is widely used surveillance technique for purely temporal processes.
#'   It is calculated by taking the cumulative sum of the standard residuals with a downward drift
#'   and a floor of 0. For a Gaussian process, residuals are calculated as `resid = wide_cases -
#'   wide_baseline`, and standard residuals are calculated as `(resid - mean)/sigma`. The CUSUM
#'   statistic is then calculated by summing the standardized residuals with downward drift `drift`.
#'
#'   The three parameters used in calculating the standardized residuals and CUSUM statistic,
#'   `mean`, `sigma`, and `drift`, can each be a scalar, a vector with a separate value for each
#'   spatial region, or a matrix with the same dimensions as `wide_cases`
#' @inherit parallel_cusum_poisson return seealso
#' @export
#' @md
#' @examples
#' outbreak_sp <- c(1, 2, 4, 5)
#' outbreak_tm <- c(3, 4)
#' wide_cases <- matrix(2, nrow = 4, ncol = 9)
#' wide_cases[outbreak_tm, outbreak_sp] <- 5
#' wide_cases[c(3, 4), c(8, 9)] <- 2.5
#' wide_baseline <- matrix(2, nrow = 4, ncol = 9)
#' scanres <- parallel_cusum_gaussian(wide_cases, wide_baseline,
#'                                    mean = 1,
#'                                    sigma = 2,
#'                                    drift = 0.5)
#'
#' v_mean <- c(rep(0.0, 8), 2)
#' m_sigma <- matrix(1.5, nrow = 4, ncol = 9)
#' m_sigma[c(3, 4), c(4, 5)] <- 0.25
#' scanres <- parallel_cusum_gaussian(wide_cases, wide_baseline,
#'                                    mean = v_mean,
#'                                    sigma = m_sigma,
#'                                    drift = 0.5)
parallel_cusum_gaussian <- function(wide_cases,
                                    wide_baseline = NULL,
                                    mean = 0,
                                    sigma = 1,
                                    drift = 0.5) {

  ### Argument checks ----
  stopifnot(is.matrix(wide_cases),
            is.null(wide_baseline) || is.matrix(wide_baseline),
            is.numeric(mean),
            is.numeric(sigma),
            is.numeric(drift))

  ### Actual computation ----
  mean_mat <- pad_cusum_inputs(mean, wide_cases)
  sigma_mat <- pad_cusum_inputs(sigma, wide_cases)
  drift_mat <- pad_cusum_inputs(drift, wide_cases)

  if (is.null(wide_baseline)) {
    residuals <- wide_cases
  } else {
    residuals <- (wide_cases - wide_baseline)
  }
  standardized_residuals <- (residuals - mean_mat) / sigma_mat


  cases_list <- mat_to_list(t(standardized_residuals))
  cusum_list <- lapply(cases_list, function(x) x * NA)
  prev_cusum <- cases_list[[1]] * 0 # We start with a CUSUM score of zero
  drift_list <- mat_to_list(t(drift_mat))

  for (date_ind in seq_len(length(cases_list))) {
    cusum_list[[date_ind]] <- pmax(prev_cusum + cases_list[[date_ind]] - drift_list[[date_ind]],
                                   0)
    prev_cusum <- cusum_list[[date_ind]]
  }

  cusum_df <- as.data.frame(cusum_list)
  wide_cusum_df <- t(cusum_df)
  colnames(wide_cusum_df) <- colnames(wide_cases)
  rownames(wide_cusum_df) <- rownames(wide_cases)
  return(as.matrix(wide_cusum_df))
}

pad_cusum_inputs <- function(x, wide_cases) {
  # Takes cusum imputs (mean, sigma, drift, or scaling), and transforms them into a matrix with the
  # same dimensions as wide_cases
  nm <- deparse(substitute(x))
  if (length(x) == 1) {
    x_mat <- matrix(x, nrow = nrow(wide_cases), ncol = ncol(wide_cases))
  } else if (is.vector(x)) {
    if (length(x) == ncol(wide_cases)) { #1 per spatial area
      x_mat <- matrix(x, nrow = nrow(wide_cases), ncol = ncol(wide_cases), byrow = TRUE)
    } else {
      stop("If ", nm, " is a vector, it must be the same length as the number of spatial areas")
    }
  } else if (is.data.frame(x) || is.matrix(x)) {
    x_mat <- as.matrix(x)
    if (!all.equal(dim(x_mat), dim(wide_cases))) { #A matrix
      stop("If ", nm,
           " is a matrix, it must have one column per spatial area and one row per time point")
    }
  } else {
    stop("Unrecognized format of x")
  }
  return(x_mat)
}
