window_spacetime <- function(spacetime_data, min_train, max_train, n_predict, model_arity) {

  spacetime_data <- dplyr::ungroup(spacetime_data)

  ### Deal with arity  ----
  if (model_arity == "uni") {
    spacetime_data = spacetime_data %>%
      dplyr::group_by(id_space)
  }


  # Ok, now our goal is to basically have a data.frame with columns
  # id_space, id_time, data, fit
  # Use the slider package to divide the data neatly up into chunks:

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

  if (model_arity == "multi") {
    spacetime_data <- spacetime_data %>%
      dplyr::group_by(id_time) %>%
      dplyr::summarize(id_space = list(id_space),
                       curr_data = list(curr_data[[1]]))
  }
  spacetime_data
}
