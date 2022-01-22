#' @title Convert a set of ordinary predictions into surveillance predictions
#'
#' @description Takes a set of data predicted by a model and removes all the data points that were
#'   used in fitting the model.
#'
#' @param list_of_dataframes A list of data.frames, organized from oldest to newest. Each data.frame
#'   should represent predictions or fitted values from a model trained on a subset of the data in
#'   the data.frame.  Each dataframe must contain columns corresponding to `space_coord` and
#'   `time_coord`, as well as 1 or more columns containing the data of interest.
#' @param n_predict For each dataframe in `list_of_dataframes`, how many time points were *not* used
#'   in fitting the model?  Can be a scalar, a vector the same length as `list_of_dataframes`, or
#'   `NULL` (see Details).
#' @param grow_length Boolean. Should the size of the returned dataframes increase as the number of
#'   available surveillance predictions increase?
#' @param return_last_only Boolean. Should the function return only the predictions for the last
#'   entry in `list_of_dataframes`, or for all entries?
#' @inheritParams loop_model
#'
#' @details
#'    Unlike ordinary model predictions, which are reported for all data points, surveillance
#'    predictions are only reported for points that were not used in calculating the model fit. For
#'    example, if a model was run at time \eqn{t} and the time points \eqn{0} to \eqn{(t-1)} were
#'    used to calculate the model fit, but model predictions were generated for time points
#'    \eqn{0} through \eqn{t}, then the only surveillance predictions would be at time point
#'    \eqn{t}.
#'
#'    Given a list of ordinary model predictions, from the same model fit sequentially on different
#'    sets of data, `convert_to_surveillance` combines surveillance predictions from multiple model
#'    fits. For each data frame in `list_of_dataframes`, the data is split into time points that
#'    were used to calculate the model fit and time points that were not. No changes are made to the
#'    data for the time points not used to calculate the model fit. For all other time points, the
#'    data is either replaced with surveillance predictions made at earlier time points, or removed
#'    the data if no predictions are available.
#'
#'    The split into time points that were and were not used in calculating the model fit is
#'    controlled by the paramater `n_predict`. If `n_predict = n`, then we assume that in each data
#'    frame, the most recent `n` time points were not used in the model fit. If `n_predict = NULL`,
#'    then we assume that all time points in a dataframe that are not contained in any earlier
#'    dataframes were not used in fitting the model.
#'
#' @return If `return_last_only = TRUE`, then a single dataframe giving the surveillance predictions
#'   for the last entry in `list_of_dataframes`. If `return_last_only = FALSE`, then a list of
#'   dataframes giving the surveillance predictions for every entry in `list_of_dataframes`. If
#'   `grow_length = FALSE`, then surveillance predictions will never be reported for a time point
#'   not included in the corresponding dataframe in `list_of_dataframes`.  If `grow_lenth = TRUE`,
#'   then each data frame will contain all of the surveillance predictions calculated up to that
#'   point.
#' @export
#' @md
#' @examples
#' library("scanstatistics")
#' library("magrittr")
#' library("gridcoord")
#' library("sf")
#' data(NM_popcas)
#' NM_popcas$county <- as.character(NM_popcas$county)
#' nm_county_coord <- statsurv::nm_county_coord
#' year_coord <- data.frame(year = 1973:1991)
#'
#' glm_func <- function(space_coord, time_coord, data_for_model) {
#'   mod <- glm(count ~ year,
#'              family = poisson(link = "log"),
#'              offset = log(population),
#'              data = data_for_model)
#'   return(list(fit = mod,
#'               data = data_for_model))
#' }
#'
#' fits_and_data <- loop_model(nm_county_coord, year_coord, NM_popcas,
#'                             outcome_col = "count",
#'                             path_to_model = glm_func)
#' all_fits <- fits_and_data[[1]]
#' all_data <- fits_and_data[[2]]
#'
#' all_yhats <- loop_extract_yhat(nm_county_coord, year_coord,
#'                                all_fits, all_data,
#'                                yhat_extractor_name = "extract",
#'                                use_surveillance_residuals = FALSE)
#'
#' # Use convert_to_surveillance to convert into surveillance predictions,
#' # allowing us to create surveillance residuals
#' convert_to_surveillance(nm_county_coord, year_coord, all_yhats,
#'                         n_predict = 1)
#'
#' # Use grow_length = TRUE, return_last_only = TRUE
#' # to generate a single data frame with all the surveillance predictions:
#' convert_to_surveillance(nm_county_coord, year_coord, all_yhats,
#'                         n_predict = 1,
#'                         grow_length = TRUE,
#'                         return_last_only = TRUE)
calculate_surveillance_residuals <- function(aug_datas,
                                             time_ids,
                                             n_predict,
                                             grow_length = FALSE) {

  ### Argument Checks ----
  # aug_data is a list of dataframes
  # check_type(aug_datas, "data.frame")
  # check_scalar_type(grow_length, "logical")


  #
  slice_time <- function(df, curr_time_id, n_predict) {
    df %>%
      filter(id_time > (curr_time_id - n_predict))
  }
  surveillance_datas <- map2(aug_datas, time_ids, slice_time, n_predict = n_predict)
  # surveillance_dfs <- fits_and_data %>%
  #   dplyr::rowwise() %>%
  #   dplyr::filter(!is.null(augmented_data)) %>%
  #   dplyr::group_by(id_space) %>%
  #   dplyr::arrange(id_time)
  #
  # take_last_n <- function(df, n) {
  #   df %>%
  #     dplyr::group_by(id_space) %>%
  #     dplyr::arrange(id_time) %>%
  #     dplyr::slice_tail(n = n) %>%
  #     dplyr::ungroup()
  # }
  surveillance_datas <- surveillance_datas %>%
    purrr::accumulate(., rbind)
    # dplyr::group_by(id_space) %>%
    # dplyr::mutate(surveillance_data = purrr::accumulate(surveillance_data, rbind))

  if (!grow_length) {
    surveillance_dats <- purrr::map2(surveillance_datas, aug_datas,
                                     function(x, y) semi_join(x, y, by = c("id_time", "id_space")))
    # surveillance_dfs <- surveillance_dfs %>%
    #   dplyr::mutate(surveillance_data = purrr::map2(surveillance_data, augmented_data,
    #                                   function(x, y) semi_join(x, y, by = c("id_time", "id_space"))))
  }


  return(surveillance_datas)
}

