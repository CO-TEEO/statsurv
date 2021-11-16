#' @title `statsurv`: A package for performing routine statistical survaliance
#'
#' @description Statistical surveillance is any sort of systematic, ongoing analysis aimed at
#'   detecting changes in outcomes as they occur.  If the
#'   amount of data reported is large or if there is significant natural variability in the rate of
#'   case events, statistical techniques can be extremely helpful in determining when a change in
#'   the number of events is important, and when it is merely noise.  The `statsurv` package is
#'   designed to help with every aspect of this process, from data collection and aggregation, to
#'   modeling, predicting, and applying alarm functions. And it is designed to simplify applying
#'   these steps consistently and repeatedly.
#'
#'@section Overview:
#'   The `statsurv` package is designed around 5 key steps statistical surveillance:
#'   \enumerate{
#'    \item Collecting and aggregating measurements of the outcome variable
#'    \item Developing a statistical model to predict the outcome variable
#'    \item Using the statistical model to predict the outcome variable in a given region or time
#'    period.
#'    \item Applying an alarm function to compare the actual outcome with the predictions, and
#'    determine the likelihood of the actual outcome happening by chance given the predictions.
#'    \item Generate reports or summaries of the surveillance outcomes over time.
#'}
#'
#'   Each of these steps has a set of associated functions in the `statsurv` package. For the
#'   intermediate steps in the process, `statsurv` provides functions for performing the step once
#'   and for performing the step repeatedly over several points in time. See
#'
#'@section Collecting and aggregating measurements:
#' #### Key functions:
#'    \itemize{
#'    \item \code{\link{generate_date_range}}
#'    \item \code{\link{generate_study_area}}
#'    \item \code{\link[gridcoord]{gc_aggregate_by}}
#'    \item \code{\link[gridcoord]{gc_rebase}}
#'    }
#'
#'@section Developing a model:
#' #### Key functions:
#' \itemize{
#'   \item \code{\link{loop_model}}
#' }
#'
#' @section Predicting outcomes:
#' #### Key functions:
#' \itemize{
#'   \item \code{\link{loop_extract_yhat}}
#'   \item \code{\link{extract_yhat}}
#'   \item \code{\link{sample_yhat}}
#' }
#'
#' #### Helper functions:
#' \itemize{
#'   \item \code{\link{collapse_yhat}}
#'   \item \code{\link{convert_to_surveillance}}
#' }
#'
#' @section Calculating alarm statistics:
#' #### Key functions:
#' \itemize{
#' \item \code{\link{loop_alarm_function}}
#' \item \code{\link{standardized_alarm_functions}}
#' \item \code{\link{standardized_alarm_multicol}}
#' }
#'
#' #### Helper functions:
#' \itemize{
#' \item \code{\link{build_key_matrix}}
#' \item \code{\link{build_zones}}
#' \item \code{\link{calculate_alarm_type}}
#' \item \code{\link{ensure_zones}}
#' \item \code{\link{parallel_cusum_gaussian}}
#' \item \code{\link{parallel_cusum_poisson}}
#' \item \code{\link{parallel_shewhart_gaussian}}
#' \item \code{\link{pivot_for_scan}}
#' \item \code{\link{scan_cusum_poisson}}
#' \item \code{\link{scan_eb_negbin_fast}}
#' \item \code{\link{scan_eb_poisson_fast}}
#' \item \code{\link{space_coord_to_zones}}
#' }
#'
#' @section Generating reports:
#' #### Key functions:
#' \itemize{
#' \item \code{\link{extract_alarm_statistic}}
#' \item \code{\link{extract_clusters}}
#' \item \code{\link{report_model_coeff}}
#' \item \code{\link{report_model_summary}}
#' }
#'
#' @section Other:
#' \itemize{
#' \item \code{\link{loop_over}}
#' }
#'
#'
#' @docType package
#' @name statsurv
#' @md
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom rlang .env
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
