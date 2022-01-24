#' Title
#'
#' @param spacetime_data
#' @param min_train
#' @param max_train
#' @param n_predict
#' @param model_arity
#'
#' @return
#' @export
#'
#' @examples
window_idtime <- function(spacetime_data, min_train,
                          max_train = Inf, n_predict = 1,
                          model_arity = c("multi", "uni")) {

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
                                                  .complete = TRUE)) %>%
    dplyr::select(id_space, id_time, curr_data) # Ditch all the other data - is that what we want to do?


  spacetime_data <- spacetime_data %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.null(curr_data)) %>%
    dplyr::ungroup()
  if (model_arity == "multi") {
    spacetime_data <- spacetime_data %>%
      dplyr::group_by(id_time) %>%
      dplyr::summarize(id_space = list(id_space),
                       curr_data = list(curr_data[[1]]))
  }
  spacetime_data
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
