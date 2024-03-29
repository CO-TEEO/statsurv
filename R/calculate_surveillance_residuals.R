#' @title Combine the most recent data points from a sequence of data frames
#'
#' @description In statistical surveillance, "surveillance residuals" are defined as the difference
#'   between observed data for a new time period and the predictions from a model fit using previous
#'   time periods. The key difference from ordinary model residuals is that surveillance residuals
#'   are never calculated for data points used to fit the model. In theory, examining surveillance
#'   residuals should allow statistical methods to detect outbreaks or clusters more rapidly, since
#'   the model used to predict baseline estimates is not affected by any current outbreaks. In
#'   practice, it is still an open question whether using surveillance residuals over ordinary
#'   residuals provides any appreciable benefit.
#'
#' `calculate_surveillance_residuals` takes a set of data frames and combines the most recent time
#' period or time periods from each one. If the data frames contain the estimates from sequentially
#' fitting a model, then the new set of data frames contains the surveillance residuals from the
#' series of model fits.
#'
#' @param list_of_dataframes A list of data frames or data frame extensions, organized from oldest
#'   to newest. Each  data frame must includes columns 'id_space' and 'id_time' that are each a set
#'   of consecutive integers. The column 'id_time' identifies the time point associated with each
#'   row, with lower numbers being earlier and higher numbers being later. The column 'id_space'
#'   identifies the spatial location or area associated with each row, but no meaning or order is
#'   ascribed to the values.
#' @param window_time_ids A vector the same length as `list_of_dataframes`. Each entry should be the
#'   value of `id_time` associated with each data frame in `list_of_dataframes`. For example, if
#'   `list_of_dataframes` contains model output, `window_time_ids` would be the time point when each
#'   model was run, or the latest timepoint included in each model.
#' @param split_ids A vector the same length of `list_of_dataframes`. Each entry should be the value
#'   of `id_time` that marks the beginning of the prediction data in each data frame.
#' @param grow_length Boolean. Should the size of the returned dataframes increase as the number of
#'   available surveillance predictions increase?
#' @param include_init Boolean. Should the output include time points that were always used in
#'   fitting the model and never had predictions generated for them? If `include_init` is TRUE and
#'   `grow_length` is false, then the dimensions of the returned data frames are all identical to
#'   the size of the input data frames.
#' @param check_space_ids Boolean. Should the function throw an error if the values of `id_space` in
#'   each data frame are not the same. Defaults to TRUE, and you should use caution before
#'   overriding.
#'
#' @details
#'    Unlike ordinary model predictions, which are reported for all data points, surveillance
#'    predictions are only reported for points that were not used in calculating the model fit. For
#'    example, if a model was run at time \eqn{t} and the time points \eqn{0} to \eqn{(t-1)} were
#'    used to calculate the model fit, but model predictions were generated for time points
#'    \eqn{0} through \eqn{t}, then the only surveillance predictions would be at time point
#'    \eqn{t}.
#'
#'    While `calculate_surveillance_residuals` can be used on any list of data frames, it is
#'    designed to used on a series of  predictions from a model fit sequentially on a growing set of
#'    data. Given a list of ordinary model predictions, from the same model fit sequentially on
#'    different sets of data, `convert_to_surveillance` combines surveillance predictions from
#'    multiple model fits.
#'
#'    If the same time period has predictions generated for it more than once, only the most recent
#'    value is reported.
#'
#' @return A list of data frames giving the surveillance predictions for every entry in
#'   `list_of_dataframes`. If `grow_length = FALSE`, then surveillance predictions will never be
#'   reported for a time point not included in the corresponding dataframe in `list_of_dataframes`.
#'   If `grow_lenth = TRUE`, then each data frame will contain all of the surveillance predictions
#'   calculated up to that point. If `include_init = TRUE`, then each data frame will also include
#'   data points that never had any predictions generated for them.
#' @export
#' @md
#' @examples
#' library("scanstatistics")
#' library("magrittr")
#' library("broom")
#' data(NM_popcas)
#'
#' # Generate a series of model predictions, each time including the data from another year:
#' spacetime_data <- NM_popcas %>%
#'   dplyr::mutate(id_time = year - min(year),
#'                 id_space = as.numeric(factor(county)))
#'
#' windowed_data <- spacetime_data %>%
#'   window_idtime(min_train = 5, max_train = 5, n_predict = 1)
#'
#' model_res <- windowed_data %>%
#'   rowmute(training_data = prepare_training_data(curr_data, count, split_id),
#'           model_fit = glm(count ~ year, family = poisson(),
#'                           offset = log(population), data = training_data),
#'           model_predictions = extract_yhat(model_fit, newdata = curr_data))
#' # The sequential model predictions are now stored in the model_res$model_predictions
#'
#' # Use convert_to_surveillance to take the last time period
#' # from each model prediction
#' calculate_surveillance_residuals(model_res$model_predictions,
#'                                  model_res$window_time_id,
#'                                  model_res$split_id)
#'
#' # Repeat with grow_length = FALSE, init = TRUE,
#' # so the dimensions of the output match the dimensions of the input
#' calculate_surveillance_residuals(model_res$model_predictions,
#'                                  model_res$window_time_id,
#'                                  model_res$split_id,
#'                                  grow_length = FALSE,
#'                                  include_init = TRUE)
#'
#' # We can use the function inside mutate to add a column to our data frame
#' model_res %>%
#'   dplyr::mutate(surveillance_predictions =
#'                    calculate_surveillance_residuals(model_predictions,
#'                                                     window_time_id,
#'                                                     split_id,
#'                                                     grow_length = FALSE,
#'                                                     include_init = TRUE))
calculate_surveillance_residuals <- function(list_of_dataframes,
                                             window_time_ids,
                                             split_ids,
                                             grow_length = FALSE,
                                             include_init = FALSE,
                                             check_space_ids = TRUE) {

  # Arg checks
  list_of_dataframes <- purrr::map(list_of_dataframes, validate_spacetime_data)
  stopifnot(rlang::is_integerish(split_ids),
            rlang::is_integerish(window_time_ids),
            rlang::is_scalar_logical(grow_length),
            rlang::is_scalar_logical(include_init),
            rlang::is_scalar_logical(check_space_ids))
  if (anyDuplicated(split_ids) | anyDuplicated(window_time_ids)) {
    stop("`split_ids` and `window_time_ids` must be unique")
  }


  # Check that id_space matches between the datas
  space_ids <- purrr::map(list_of_dataframes, function(x) sort(unique(x$id_space)))
  if (length(unique(space_ids)) != 1 && check_space_ids) {
    stop("space_ids do not match between entires in `list_of_dataframes`. ",
         "Either run with `check_space_ids = FALSE` or run on a subset of the data frames")
  }
  # Hack to avoid notes on R Cmd Check
  id_time <- NULL; split_id <- NULL; working_data <- NULL; surveillance_data <- NULL
  # Actual code

  # Put the data into a data frame
  working_df <- tibble::tibble(window_time_id = window_time_ids,
                               split_id = split_ids,
                               working_data = list_of_dataframes) %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::arrange(.data$window_time_id)

  working_df <- working_df %>%
    rowmute(surveillance_data = dplyr::filter(.data$working_data,
                                              id_time >= split_id))



  filter_and_bind <- function(x, y) {
    # If we have overlapping predictions between steps, always take the most recent.
    time_inds <- unique(y$id_time)
    x2 <- dplyr::filter(x, !.data$id_time %in% .env$time_inds)
    rbind(x2, y)
  }


  if (include_init) {
    init <- working_df %>%
      dplyr::slice_min(split_id) %>%
      rowmute(init = dplyr::filter(working_data, id_time < split_id)) %>%
      dplyr::pull(init) %>%
      .[[1]]

    working_df <- working_df %>%
      dplyr::mutate(surveillance_data = purrr::accumulate(.data$surveillance_data, filter_and_bind,
                                                          .init = init)[-1])
  } else {
    working_df <- working_df %>%
      dplyr::mutate(surveillance_data = purrr::accumulate(.data$surveillance_data, filter_and_bind))

  }



  if (!grow_length) {
    working_df <- working_df %>%
      rowmute(surveillance_data = dplyr::semi_join(surveillance_data, working_data,
                                                   by = c("id_time", "id_space")))
  }

  surveillance_datas <- working_df %>%
    dplyr::arrange(.data$row_id) %>%
    dplyr::pull(.data$surveillance_data)
  return(surveillance_datas)
}

