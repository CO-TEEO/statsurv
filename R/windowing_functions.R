#' Divide data into overlapping windows
#'
#' This function divides a dataframe into overlapping time windows, in preparation for repeatedly
#' applying statistical surveillance methods. Each window consists of 0 or more data points to fit a
#' statistical model on, and 0 or more points to generate predictions for.
#'
#'
#' @inheritParams spacetime_data
#' @param min_train,max_train The minimum and maximum number of time points (as identified by
#'   `id_time`) to include in the training data set. Times with fewer than this number of time
#'   points will not be included as windows. Later times will include more time points in the
#'   training data set, up to the maximum number identified by `max_train`.
#' @param n_predict The number of time points (as identified by `id_time`) to have predictions
#'   generated for.
#' @param step How often should a window be generated? Default is 1, so a window is generated at
#'   every value of `id_time`.
#' @param split_spatial_locations Whether each spatial area (as identified by `id_space`) should be
#'   located in its own data window (if `split_spatial_locations = TRUE`) or if they should all be
#'   included in one window (if `split_spatial_locations = "FALSE"`).
#'
#' @return A tibble with one row per data window, containing the columns `id_time`, `id_space`,
#'   `split_id`, and `curr_data`. `id_time` and `id_space` identify the time point defining the end
#'   of the time point and the spatial area or areas associated with the data window.  `split_id` is
#'   the first time point in the data window in the prediction set. The data for each window is
#'   stored in the `curr_data` column. There is no difference in the windowed data between data in
#'   the training set and the prediction set, but that information is used by other `statsurv`
#'   functions.
#' @export
#' @md
#'
#' @examples
#' x = rnorm(100)
#' spacetime_data <- data.frame(id_space = rep(1:10, each = 10),
#'                              id_time = rep(1:10, 10),
#'                              x = x,
#'                              y = 2.04 * x + 1.23)
#'
#' # Create overlapping groups of data with at least 4 time points
#' # and up to 8 time points in the
#' # training set, and 2 time points in the prediction set.
#' window_idtime(spacetime_data, min_train = 4, max_train = 8,
#'               n_predict = 2, split_spatial_locations = TRUE)
#'
#' # The same, but put each spatial area in its own window
#' window_idtime(spacetime_data, min_train = 4, max_train = 8,
#'               n_predict = 2, split_spatial_locations = TRUE)
window_idtime <- function(spacetime_data, min_train,
                          max_train = Inf, n_predict = 1,
                          step = 1,
                          split_spatial_locations = FALSE) {

  # Arg checks
  spacetime_data <- validate_spacetime_data(spacetime_data)
  stopifnot(rlang::is_scalar_integerish(min_train),
            rlang::is_scalar_integerish(max_train),
            rlang::is_scalar_integerish(n_predict),
            rlang::is_scalar_integerish(step),
            step >= 1,
            rlang::is_scalar_logical(split_spatial_locations))

  n_times <- length(unique(spacetime_data$id_time))
  stopifnot((min_train + n_predict) <= n_times)
  stopifnot(min_train > 0,
            n_predict >= 0,
            max_train >= min_train,
            min_train + n_predict <= nrow(spacetime_data))


  # Actual code
  spacetime_data <- dplyr::ungroup(spacetime_data)

  ### Deal with arity  ----
  if (split_spatial_locations) {
    spacetime_data <- spacetime_data %>%
      dplyr::group_by(.data$id_space)
  }


  # Ok, now our goal is to basically have a data.frame with columns
  # id_time, id_space, data
  # We use the slider package to divide the data neatly up into chunks:

  # We have an irregular fitting window, so need a special function to
  # define the before index
  before_func <- function(x) {
    ifelse(x < (min_train + n_predict),
           -1,
           pmax(x - (max_train + n_predict) + 1, 1))
  }
  spacetime_data <-
    spacetime_data %>%
    dplyr::arrange(.data$id_time) %>%
    dplyr::mutate(curr_data = slider::slide_index(dplyr::cur_data_all(), .data$id_time,
                                                  function(df) df,
                                                  .before = before_func,
                                                  .after = 0,
                                                  .complete = TRUE),
                  split_id = .data$id_time - n_predict + 1) %>%
    dplyr::select(.data$id_space, .data$id_time, .data$split_id, .data$curr_data)


  spacetime_data <- spacetime_data %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.null(.data$curr_data)) %>%
    dplyr::ungroup()
  if (!split_spatial_locations) {
    spacetime_data <- spacetime_data %>%
      dplyr::group_by(.data$id_time) %>%
      dplyr::summarize(id_space = list(.data$id_space),
                       curr_data = list(.data$curr_data[[1]]),
                       split_id = .data$split_id[[1]])
  }

  # Clean up the names
  spacetime_data <- spacetime_data %>%
    dplyr::select(window_time_id = .data$id_time,
                  window_space_id = .data$id_space,
                  split_id = .data$split_id,
                  curr_data = .data$curr_data)

  if (step != 1) {
    wanted_time_ids <- seq(min(spacetime_data$window_time_id), max(spacetime_data$window_time_id),
                           by = step)
    spacetime_data <- spacetime_data %>%
      dplyr::filter(.data$window_time_id %in% wanted_time_ids)
  }

  spacetime_data
}

