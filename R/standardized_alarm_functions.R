#' @title A standardized interface for calling alarm functions
#'
#' @description Call any of 12 alarm functions with a consistent interface.
#' @param alarm_function_name The name of the alarm function to call. See the list of supported
#'   alarm functions below.
#' @param zone_info Either:
#'    \itemize{
#'      \item A list of integer vectors. Each vector corresponds to a single
#'    zone; its elements are the numbers of the locations in that zone.
#'      \item A matrix specifying how which locations belong to each zone. Each row is a different
#'      zone and each column is a location. An element `(i,j)` is 1 if location `j` is part of zone
#'      `i` and is 0 otherwise. Can be generated from a list specifying zones using the
#'      \code{\link{build_key_matrix}} function.
#'      }
#' @param wide_baseline A matrix of the same dimensions as counts, holding the expected value for
#'   each observed space-time location. This value is typically estimated from past data using a GLM
#'   or other model. This parameter is passed into the `population` argument for
#'   \code{\link[scanstatistics]{scan_pb_poisson}} and
#'   \code{\link[scanstatistics]{scan_permutation}}, and into the `baselines` or `wide_baselines`
#'   argument for all other alarm functions.
#'
#' @param ... Additional parameters to pass to the specified alarm function.
#' @inheritParams scan_eb_negbin_fast
#'
#' @section Supported Alarm Functions:
#'   The following alarm functions are supported:
#'   \itemize{
#'     \item{\code{\link[scanstatistics]{scan_eb_poisson}}}
#'     \item{\code{\link[scanstatistics]{scan_pb_poisson}}}
#'     \item{\code{\link[scanstatistics]{scan_eb_negbin}}}
#'     \item{\code{\link[scanstatistics]{scan_eb_zip}}}
#'     \item{\code{\link[scanstatistics]{scan_permutation}}}
#'     \item{\code{\link[scanstatistics]{scan_bayes_negbin}}}
#'     \item{\code{\link{scan_eb_poisson_fast}}}
#'     \item{\code{\link{scan_eb_negbin_fast}}}
#'     \item{\code{\link{scan_cusum_poisson}}}
#'     \item{\code{\link{parallel_cusum_poisson}}}
#'     \item{\code{\link{parallel_cusum_gaussian}}}
#'     \item{\code{\link{parallel_shewhart_gaussian}}}
#'     }
#' @return
#'   For the alarm functions that begin "parallel_", a matrix with the same dimensions as
#'   `wide_cases`; for all other alarm functions an object of class `scanstatistics`. All results
#'   have an attribute "alarm_type" describing whether they are a scan-type alarm function or a
#'   parallel-type alarm function. All returned `scanstatistics` objects have been
#'   standardized as follows:
#'   \itemize{
#'   \item{The returned object now contains the component `zone_info` containing information about
#'   the spatio-temporal zones}
#'   \item{The returned object always contains the component `observed`, giving the likelihood of an
#'   outbreak in each spatio-temporal zone},
#'   \item{The components `observed`, `MLC`, and `replicates` (if applicable) always contain the
#'   column `action_level`. The higher the value of this column, the greater the likelihood of an
#'   outbreak. This column is identical to `score` for all scan-type alarm functions except for
#'   \code{\link[scanstatistics]{scan_bayes_negbin}}, where it is equal to `log_posterior`}
#'   }
#' @md
#' @examples
#' # Apply scan_cusum_poisson to a 3x3 spatial grid with 4 time points
#' # Inject an outbreak into the upper-left corner in
#' # the 2 most recent time points
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
#' wide_baseline <- matrix(2, nrow = 4, ncol = 9)
#'
#' standardized_alarm_functions("scan_eb_poisson",
#'                              wide_cases, zones, wide_baseline,
#'                              n_mcsim = 10)
#' standardized_alarm_functions("scan_permutation",
#'                              wide_cases, zones, wide_baseline,
#'                              n_mcsim = 10)
#' standardized_alarm_functions("scan_eb_negbin_fast",
#'                              wide_cases, zones, wide_baseline,
#'                              thetas = 1.5, n_mcsim = 10)
standardized_alarm_functions <- function(alarm_function_name, wide_cases, zone_info, wide_baseline,
                                        ..., n_mcsim) {

  requires_zones <- c("scan_eb_poisson",
                      "scan_pb_poisson",
                      "scan_eb_negbin",
                      "scan_eb_zip",
                      "scan_permutation",
                      "scan_bayes_negbin")
  requires_key <- c("scan_eb_poisson_fast", "scan_cusum_poisson", "scan_eb_negbin_fast")
  scan_type <- c(requires_zones, requires_key)

  parallel_type <- c("parallel_cusum_poisson",
                     "parallel_cusum_gaussian",
                     "parallel_shewhart_gaussian")

  all_alarms <- c(scan_type, parallel_type)

  ### Argument checks ----
  if (alarm_function_name %in% scan_type) {
    if (alarm_function_name != "scan_bayes_negbin") {
      check_scalar_type(n_mcsim, "numeric")
    }
    if (alarm_function_name %in% requires_key) {
      if (is.list(zone_info)) {
        key_matrix <- build_key_matrix(zone_info)
      } else {
        key_matrix <- zone_info
      }
    } else if (alarm_function_name %in% requires_zones) {
      if (is_type(zone_info, c("matrix", "Matrix"))) {
        zones <- build_zones(zone_info)
      } else {
        zones <- zone_info
      }
    } else {
      stop("zone_info must either be a matrix or a list specifying ",
           "which locations belong to each zone")
    }
  }

  if (alarm_function_name %in% scan_type) {
    alarm_type <- "scan"
  } else if (alarm_function_name %in% parallel_type) {
    alarm_type <- "parallel"
  } else {
    stop("Unrecognized scan_function_name. Currently recognized names are ",
         all_alarms)
  }
  # All other arguments checked in their respective functions

  ### Apply alarm function ----
  scan_type1 <- c("scan_eb_poisson", "scan_pb_poisson", "scan_eb_negbin",
                  "scan_eb_zip", "scan_permutation")
  if (alarm_function_name %in% scan_type1) {
    f_to_use <- switch(alarm_function_name,
                       "scan_eb_poisson" = scanstatistics::scan_eb_poisson,
                       "scan_pb_poisson" = scanstatistics::scan_pb_poisson,
                       "scan_eb_negbin" = scanstatistics::scan_eb_negbin,
                       "scan_eb_zip" = scanstatistics::scan_eb_zip,
                       "scan_permutation" = scanstatistics::scan_permutation)
    alarm_res <- f_to_use(wide_cases, zones, wide_baseline, n_mcsim = n_mcsim, ...)
  } else if (alarm_function_name == "scan_bayes_negbin") {
    alarm_res <- scanstatistics::scan_bayes_negbin(wide_cases, zones, wide_baseline, ...)
  } else if (alarm_function_name == "scan_eb_poisson_fast") {
    alarm_res <- scan_eb_poisson_fast(wide_cases, key_matrix, wide_baseline, n_mcsim = n_mcsim)
  } else if (alarm_function_name == "scan_eb_negbin_fast") {
    alarm_res <- scan_eb_negbin_fast(wide_cases, key_matrix, wide_baseline, n_mcsim = n_mcsim, ...)
  } else if (alarm_function_name == "scan_cusum_poisson") {
    alarm_res <- scan_cusum_poisson(wide_cases, key_matrix, wide_baseline, n_mcsim = n_mcsim, ...)
  } else if (alarm_function_name %in% parallel_type) {
    f_to_use <- switch(alarm_function_name,
                       "parallel_cusum_poisson" = parallel_cusum_poisson,
                       "parallel_cusum_gaussian" = parallel_cusum_gaussian,
                       "parallel_shewhart_gaussian" = parallel_shewhart_gaussian)
    alarm_res <- f_to_use(wide_cases, wide_baseline, ...)
  } else {
    browser()
    stop("Unrecognized alarm function")
  }

  # A little work to standardize the results:
  # Including the zone_info
  alarm_res <- standardize_alarm(alarm_res, alarm_type)
  if (alarm_type == "scan") {
    alarm_res$zone_info <- zone_info
  }
  return(alarm_res)
}


standardize_alarm <- function(alarm_res, alarm_type) {
  attr(alarm_res, "alarm_type") <- alarm_type
  if (alarm_type == "scan") {
    if (!is.null(alarm_res$posteriors)) { #Check for scan_bayes_negbin
      alarm_res$observed <- alarm_res$posteriors$window_posteriors
      alarm_res$observed$action_level <- alarm_res$observed$log_posterior
      alarm_res$MLC$action_level <- alarm_res$MLC$log_posterior
    } else {
      alarm_res$observed$action_level <- alarm_res$observed$score
      alarm_res$MLC$action_level <- alarm_res$MLC$score
      alarm_res$replicates$action_level <- alarm_res$replicates$score
    }
  }
  return(alarm_res)
}
