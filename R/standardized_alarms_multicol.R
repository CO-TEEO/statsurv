#' @title A standardized interface for calling alarm functions with multiple baseline samples
#'
#' @description Call any of 12 alarm functions with a consistent interface, with a different
#'   baseline sample each time.
#' @param space_coord A gridcoord object (\code{\link[gridcoord]{gc_gridcoord}}) describing the
#'   spatial area that is covered by `long_yhat`.
#' @param time_coord A gridcoord object (\code{\link[gridcoord]{gc_gridcoord}})
#'   describing the temporal area that is to be covered by `long_yhat`. This coordinate must be
#'   ordered, with the first entries in the dataframe corresponding to the earliest time periods and
#'   the last entries corresponding to the most recent.
#' @param long_yhat A data.frame containing information about the expected number of cases in each
#'   space-time location. The first two columns of the data.frame must correspond to the space
#'   coordinate and time coordinate of the observations. The remaining 1 or more columns should
#'   contain information about the expected number of cases in each space-time location. These
#'   observations can either be point estimates or random samples, and can be generated from a model
#'   fit object using the functions \code{\link{extract_yhat}} or \code{\link{sample_yhat}}.
#' @param n_mcsim A non-negative integer; the number of replicate scan statistics to generate for
#'   each baseline sample in order to calculate a P-value. The total number of replicates is equal
#'   to the number of basline samples times `n_mcsim`
#' @inheritParams loop_model
#' @inheritParams standardized_alarm_functions
#'
#' @details `standardized_alarm_multicol` provides a way to apply an alarm function to multiple
#'   baseline samples and summarize the result. This is potentially useful for calculating p-values
#'   in cases when the baseline samples in different locations are highly correlated. The requested
#'   alarm function is run independently for each baseline sample and the results are then
#'   aggregated to provide a single estimate of the likelihood of an outbreak in any given zone.
#'
#' @note The format for the observed and baseline inputs to `standardized_alarm_multicol` are not
#'   the same. The observed number of cases (`wide_cases`) must be provided as a wide matrix, where
#'   each row indciates time and each column indicated spatial location. In contrast, the baseline
#'   information (`long_yhat`) must be provided as a long (tidy) data.frame, where each row in the
#'   data.frame identifies a unique space-time location.
#'
#' @inheritSection standardized_alarm_functions Supported Alarm Functions
#' @return Identical in format to the value returned by the individual alarm function called.
#'   Typically either an object of class `scanstatistics` or a matrix the same dimensions as
#'   `wide_cases`. In either case, the returned object summarizes the information from every
#'   baseline sample: scores in each zone are averaged, and replicate values are concatenated.
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
#' outbreak_sp <- c(1, 2, 4, 5)
#' outbreak_tm <- c(3, 4)
#' wide_cases <- matrix(2, nrow = 4, ncol = 9)
#' wide_cases[outbreak_tm, outbreak_sp] <- 5
#' wide_cases[c(3, 4), c(8, 9)] <- 3
#' space_coord <- data.frame("space" = paste0("s", 1:9), stringsAsFactors = FALSE)
#' time_coord <- data.frame("time" = paste0("t", 1:4), stringsAsFactors = FALSE)
#'
#' tall_baseline <- expand.grid(space = space_coord$space,
#'                              time = time_coord$time)
#' tall_baseline$sample1 <- 2
#' tall_baseline$sample2 <- 2.12
#'
#' standardized_alarm_multicol(space_coord, time_coord, "scan_eb_poisson",
#'                              wide_cases, zones, tall_baseline, n_mcsim = 10)
#' standardized_alarm_multicol(space_coord, time_coord, "scan_bayes_negbin",
#'                              wide_cases, zones, tall_baseline, n_mcsim = 10)
#' standardized_alarm_multicol(space_coord, time_coord, "parallel_cusum_poisson",
#'                              wide_cases, zones, tall_baseline, n_mcsim = 10)
call_alarm_function <- function(alarm_function_name,
                                spacetime_data, outcome_col,
                                zone_info, n_mcsim, ...) {

  ### Argument Checks ----
  # validate_yhat(space_coord, time_coord, long_yhat)
  # All other args passed through to standardized_alarm_functions
  wide_cases <- pivot_for_scan(spacetime_data, outcome_col)

  cn <- colnames(spacetime_data)
  baseline_columns <- which(startsWith(cn, ".fitted") | startsWith(cn, ".sample"))
  alarm_accumulator <- list()
  ii <- 0
  for (ind in baseline_columns) {
    wide_baseline <- pivot_for_scan(spacetime_data,
                                    cn[[ind]])

    alarm_res <- standardized_alarm_functions(alarm_function_name, wide_cases, zone_info, wide_baseline,
                                              n_mcsim = n_mcsim, ...)
    ii <- ii + 1
    alarm_accumulator[[ii]] <- alarm_res
  }
  alarm_output <- collapse_alarm_functions(alarm_accumulator, alarm_function_name)

  if (calculate_alarm_type(alarm_function_name) == "parallel") {
    alarm_output <- unpivot_parallel_alarms(alarm_output, spacetime_data, ".action_level")
  }
  return(alarm_output)
}
