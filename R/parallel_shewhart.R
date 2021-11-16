#' @title Calculate the Gaussian Shewhart alarm statistic over multiple spatial regions
#'
#' @description `parallel_cusum_gaussian` calculates the value of the Shewhart alarm statistic
#'   assuming a Gaussian distribution independently over each spatial region, over all time points.
#'   The Shewhart alarm statistic considers each spatial region and time point independently, and
#'   information from multiple observations is never aggregated.
#'
#' @inheritParams parallel_cusum_gaussian
#'
#' @details The Shewhart alarm statistic is equal to the number of standard deviations that a given
#'   observation is above or below the mean, also known as a z-score. It can be directly calculated
#'   as `(wide_cases - wide_baseline - mean) / sd`.
#'
#' @return A matrix with the same dimensions as `wide_cases`, where rows indicate time (oldest to
#'   newst) and columns indicate locations. Element `(i, j) ` indicates the value of the Shewhart
#'   alarm statistic at time `i` in region `j`. The value of the Shewhart statistic at row `i`
#'   incorporates informations only from time point `i`.
#'
#' @export
#' @md
#' @examples
#' outbreak_sp <- c(1, 2, 4, 5)
#' outbreak_tm <- c(3, 4)
#' wide_cases <- matrix(2, nrow = 4, ncol = 9)
#' wide_cases[outbreak_tm, outbreak_sp] <- 5
#' wide_cases[c(3, 4), c(8, 9)] <- 2.5
#' wide_baseline <- matrix(2, nrow = 4, ncol = 9)
#' scanres <- parallel_shewhart_gaussian(wide_cases, wide_baseline,
#'                                       mean = 1,
#'                                       sigma = 0.6)
#'
#' v_mean <- c(rep(0.0, 8), 2)
#' m_sigma <- matrix(1.5, nrow = 4, ncol = 9)
#' m_sigma[c(3, 4), c(4, 5)] <- 0.25
#' scanres <- parallel_shewhart_gaussian(wide_cases, wide_baseline,
#'                                       mean = v_mean,
#'                                       sigma = m_sigma)
parallel_shewhart_gaussian <- function(wide_cases,
                                       wide_baseline = NULL,
                                       mean = 0,
                                       sigma = 1) {
  # wide_cases we expect to be a matrix where columns are the locations and rows are the time
  # periods (oldest to newest)
  # For consistency each alarm function tests things exactly once.
  mean_mat <- pad_cusum_inputs(mean, wide_cases)
  sigma_mat <- pad_cusum_inputs(sigma, wide_cases)
  if (is.null(wide_baseline)) {
    residuals <- wide_cases
  } else {
    residuals <- (wide_cases - wide_baseline)
  }
  standardized_residuals <- (residuals - mean_mat) / sigma_mat

  shewhart_df <- as.data.frame(standardized_residuals)
  colnames(shewhart_df) <- colnames(wide_cases)
  rownames(shewhart_df) <- rownames(wide_cases)
  return(shewhart_df)
}

