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
#' @param model_arity Whether each spatial area (as identified by `id_space`) should be located in
#'   its own data window (if `model_arity = "uni"`) or if they should all be included in one window
#'   (if `model_arith = "multi"`).
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
#' Create overlapping groups of data with at least 4 time points and up to 8 time points in the
#' training set, and 2 time points in the prediction set.
#' window_idtime(spacetime_data, min_train = 4, max_train = 8,
#'               n_predict = 2, model_arity = "multi")
#'
#' The same, but put each spatial area in its own window
#' window_idtime(spacetime_data, min_train = 4, max_train = 8,
#'               n_predict = 2, model_arity = "uni")
window_idtime <- function(spacetime_data, min_train,
                          max_train = Inf, n_predict = 1,
                          model_arity = c("multi", "uni")) {

  #TOOD(): Add a "step" argument, so we can run every n steps

  # Arg checks
  spacetime_data <- validate_spacetime_data(spacetime_data)
  model_arity <- match.arg(model_arity)
  stopifnot(rlang::is_scalar_integerish(min_train),
            rlang::is_scalar_integerish(max_train),
            rlang::is_scalar_integerish(n_predict))

  n_times <- length(unique(spacetime_data$id_time))
  stopifnot((min_train + n_predict) <= n_times)
  stopifnot(min_train > 0,
            n_predict >= 0,
            max_train >= min_train,
            min_train + n_predict <= nrow(spacetime_data))


  # Actual code
  spacetime_data <- dplyr::ungroup(spacetime_data)

  ### Deal with arity  ----
  if (model_arity == "uni") {
    spacetime_data = spacetime_data %>%
      dplyr::group_by(id_space)
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
    dplyr::arrange(id_time) %>%
    dplyr::mutate(curr_data = slider::slide_index(dplyr::cur_data_all(), id_time,
                                                  function(df) df,
                                                  .before = before_func,
                                                  .after = 0,
                                                  .complete = TRUE),
                  split_id = id_time - n_predict + 1) %>%
    dplyr::select(id_space, id_time, split_id, curr_data)


  spacetime_data <- spacetime_data %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.null(curr_data)) %>%
    dplyr::ungroup()
  if (model_arity == "multi") {
    spacetime_data <- spacetime_data %>%
      dplyr::group_by(id_time) %>%
      dplyr::summarize(id_space = list(id_space),
                       curr_data = list(curr_data[[1]]),
                       split_id = split_id[[1]])
  }
  spacetime_data %>%
    dplyr::select(id_time, id_space, split_id, curr_data)
}

window_date <- function(spacetime_data, min_train, max_train, predict_window, model_arity) {
  # Is this well-specified?
  # We want to include all data in the past n days as time window, and we want to include the
  # next m days as predictors
  # But...hrm.
  # I need to be careful about that, because then filtering the n_predict is going to be difficult.
  # That makes the data prep functions all pretty tricky.
  # Is there a way I can make that easier?
  # Ok, there are really two id_times that we need to keep track of.
  # That feels like it should be added to the data frame.
  # If we had that, I think we could simplify some of the other functions.

}
