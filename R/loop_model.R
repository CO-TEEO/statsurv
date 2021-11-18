#' @title Fit a model over multiple time windows
#'
#' @description \code{loop_model} fits a model to multiple sequential time points, optionally also
#'   over multiple spatial regions
#'
#' @param space_coord A gridcoord object
#'   (\code{\link[gridcoord]{gc_gridcoord}}) describing the spatial area that is
#'   covered by \code{data_for_model}.
#' @param time_coord A gridcoord object (\code{\link[gridcoord]{gc_gridcoord}})
#'   describing the temporal area that is to be covered by the model. This coordinate must be
#'   ordered, with the first entries in the dataframe corresponding to the earliest time periods and
#'   the last entries corresponding to the most recent.
#' @param data_for_model A data frame containing all of the covariate information required by the
#'   model. In addition, \code{data_for_model} must have two columns with the same column name and
#'   entries as the left-most columns in \code{space_coord} and \code{time_coord} columns.
#' @param outcome_col The name of the column in \code{data_for_model} containing the primary outcome
#'   (e.g., the number of elevated cases).
#' @param path_to_model The path to the model function, or the function itself. If a path to the
#'   model function, it must point to a .R file containing a single function. See the Details
#'   section or `vignette("model-functions")` for a more in-depth explanation.
#' @param min_train The minimum number of time points to include in each model fit
#' @param max_train The maximum number of time poitns to include in each model fit
#' @param n_predict How many time points should be predicted each time the model is fit
#' @param model_arity Can the model handle multiple spatial regions as once (\code{"multi"}) or only
#'   one (\code{ "uni"})
#' @param use_cache Should the results from this function be saved (cached), so that the results can
#'   be quickly loaded without having to fit a (possibly very complicated) model.
#' @param force If previous results have been cached, should the function be forced to redo the
#'   calculations? \code{force} can be a single logical value or a vector indicating specific time
#'   points to be re-calculated.
#' @param save_environments Should any environments captured by formulas or model fits be included
#'   in the saved results? See the `Caching` section for details.
#' @param verbose Should updates be printed to the console?
#' @param extra_model_args A named list containing additional parameters to supply to the model
#'   function. See \code{vignette("model-functions")} for an oveview of how this works.
#'
#'
#' @section Model fit functions:
#' `loop_model` repeatedly fits a model function to data, but supplying the model function is up to
#' the user. The model must be supplied in the form of a function whose first three arguments are
#' `space_coord`, `time_coord`, and `data_for_model`. The function must return a list,  where the
#' first element is the model fit object and is named "fit", and the second element is the
#' data.frame used by the fitting function and is named "data". The function can take any number of
#' additional arguments, which can be passed to the model function by including them as entries in
#' the `extra_model_args` parameter, and it can return any number of additional values by including
#' them as extra entries in the returned list.
#'
#'
#' @section Caching overview:
#'   Caching in the `statsurv` is done using the \code{\link[simplecache:cache_wrap]{simplecache}}
#'   package. The first time you run this function with `use_cache = TRUE`, it will create a
#'   sub-directory based on the value of `path_to_model`, where it will save the returned values
#'   from every iteration. The next time you run the function, `simplecache` will check if the
#'   function inputs are identical to a previous run. If so, `simplecache` will load the model
#'   output from disc instead of re-fitting the model.
#'
#'   Determing when to use the cache is not always straightforward, so it is possible that
#'   `simplecache` will make a mistake sometimes and use a cached value when it should not. The
#'   `force` argument to allows you to override the default behavior, and force the function to
#'   re-calculate the results from scratch.
#'
#'   Some functions take an additional argument `save_environments` that controls how fit objects
#'   and formulas as saved. These objects often capture their entire environment when they are
#'   created, which can cause the saved objects to be extremely large. Setting
#'   \code{save_environments = FALSE} removes the captured environments from the model fits when
#'   saving them.
#'
#'
#' @return A hierarchically organized list. At the top level, the returned list will contain the
#'   components "fit", "data", and any additional values returned by the model function. If
#'   \code{model_arity = "multi"} or there was only one spatial region, then each component will be
#'   a list organized by time. If \code{model_arity = "uni"}, then each component will be a
#'   \link[gridcoord]{gridlist}, organized first by spatial region and then by time.
#'
#' @examples
#' \dontrun{
#' # Define our coordinates and generate data
#' space_coord <- data.frame(space_label = c("space1", "space2"),
#'                           stringsAsFactors = FALSE)
#' time_coord <- data.frame(time_label = paste0("t", 1:10),
#'                          time = 1:10,
#'                          fin_time = 2:11,
#'                          stringsAsFactors = FALSE)
#' data_for_model <- expand.grid(space_label = space_coord[[1]],
#'                               time_label = time_coord[[1]],
#'                               stringsAsFactors = FALSE)
#' data_for_model$x <- runif(nrow(data_for_model), max = 15)
#' data_for_model$y <- data_for_model$x * 1.05 +
#'                         rnorm(nrow(data_for_model))
#'
#' # Define our model function
#' simple_lm_func <- function(space_coord,
#'                            time_coord,
#'                            data_for_model) {
#'   fit <- lm(y ~ x,
#'             data = data_for_model)
#'    return(list(fit = fit,
#'               data = data_for_model))
#' }
#'
#' # Run the model as repeated surveillance
#' loop_model(space_coord, time_coord, data_for_model, "y",
#'            simple_lm_func,
#'            min_train = 5,
#'            use_cache = FALSE)
#'
#' # Run the same model, but fit it
#' # seperately to each spatial region
#' loop_model(space_coord, time_coord, data_for_model, "y",
#'            simple_lm_func,
#'            min_train = 5,
#'            use_cache = FALSE,
#'            model_arity = "uni")
#' }
#' @family looping functions
#' @export
#' @md
loop_model <- function(spacetime_data,
                       outcome_col,
                       model_function,
                       data_prep_function = NULL,
                       min_train = 7,
                       max_train = Inf,
                       n_predict = 1,
                       step = 1,
                       model_arity = c("multi", "uni"),
                       prediction_strategy = c("NA", "truncate"),
                       ...) {

  extra_args <- list(...)

  ### Helper functions ----
  make_data_for_model <- function(curr_data, curr_time_index, outcome_col, prediction_strategy) {
    mask <- curr_data$id_time > curr_time_index

    if (prediction_strategy == "NA") {
      curr_data[mask, outcome_col] <- NA
    } else if (prediction_strategy == "truncate") {
      curr_data <- curr_data[!mask, ]
    } else {
      stop("Invalid value for 'prediction_strategy'")
    }
    return(curr_data)
  }



    make_data_for_model <- skipping_nulls(make_data_for_model)


  ### Argument checks ----
  # space_coord <- gridcoord::gc_gridcoord(space_coord)
  # time_coord <- gridcoord::gc_gridcoord(time_coord)
  check_type(spacetime_data, "data.frame")
  if (!"id_space" %in% colnames(spacetime_data) || !"id_time" %in% colnames(spacetime_data)) {
    stop_subclass("spacetime_data must have numeric columns 'id_space' and 'id_time' identifying the space and time coordinates")
  }
  check_type(spacetime_data$id_space, "integer")
  check_type(spacetime_data$id_time, "integer")

  check_scalar_type(outcome_col, "character")
  check_scalar_type(model_function, "function") # Fewer options makes life easier
  check_type(data_prep_function, c("NULL", "function"))
  check_scalar_type(min_train, "integer")
  check_scalar_type(max_train, "integer")
  if (min_train < 1) {
    stop_subclass("min_train must be a positive integer", .subclass = "error_bad_arg_value")
  }
  if (min_train >= max(spacetime_data$id_time)) {
    stop_subclass("min_train is greater than or equal to the number of distinct time points. ",
                       "Unable to calculate any model fits.", .subclass = "error_bad_arg_value")
  }
  if (max_train < min_train) {
    stop_subclass("max_train must be greater than or equal to min_train",
                  .subclass = "error_bad_arg_value")
  }
  check_scalar_type(n_predict, "integer")
  model_arity <- match.arg(model_arity)
  prediction_strategy <- match.arg(prediction_strategy)



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
    ifelse(x < min_train,
           -1,
           pmax(x - max_train + 1, 1))
  }
  spacetime_data <-
    spacetime_data %>%
    dplyr::arrange(id_time) %>%
    dplyr::mutate(curr_data = slider::slide_index(dplyr::cur_data_all(), id_time,
                                   function(df) df,
                                   .before = before_func,
                                   .after = n_predict,
                                   .complete = TRUE)) %>%
    dplyr::select(id_space, id_time, curr_data) # Ditch all the other data - is that what we want to do?

  if (model_arity == "multi") {
    spacetime_data <- spacetime_data %>%
      dplyr::group_by(id_time) %>%
      dplyr::filter(dplyr::row_number() == 1)
  }

  # Then use the prediction_strategy to come up with a new data to feed into the model:
  spacetime_data <- spacetime_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(data_for_model = list(make_data_for_model(curr_data, id_time, outcome_col, prediction_strategy)))


  if (!is.null(data_prep_function)) {
    data_prep_function2 <- skipping_nulls(data_prep_function)
    spacetime_data <- spacetime_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(args_for_model = list(ensure_list(data_prep_function2(data_for_model))))
  } else {
    spacetime_data <- spacetime_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(args_for_model = list(list(data_for_model)))
  }


  spacetime_data <- spacetime_data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.row_id = dplyr::row_number())

  # Do some work to figure out what rows to run on:
  wanted_row_ids <- spacetime_data %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.null(curr_data)) %>%
    dplyr::group_by(id_space) %>%
    dplyr::filter((dplyr::row_number() - 1) %% step == 0) %>%
    dplyr::pull(.row_id)

  p <- progressr::progressor(length(wanted_row_ids))

  fits <- rep(list(NULL), nrow(spacetime_data))
  for (i in wanted_row_ids) {
    curr_row <- spacetime_data[i, ]
    if (identical(curr_row$curr_data, list(NULL))) {
      fits[i] <- list(NULL)
    } else {
      fits[[i]] <- rlang::exec(model_function, !!!spacetime_data$args_for_model[[i]], !!!extra_args)
    }
    p()
  }

  spacetime_data$model_fit = fits
  spacetime_data <- spacetime_data %>%
    dplyr::ungroup() %>%
    dplyr::rename(training_data = data_for_model,
                  curr_data = curr_data) %>%
    dplyr::select(-args_for_model, -.row_id) %>%
    dplyr::mutate(.n_predict = n_predict, .before = curr_data)
  return(spacetime_data)
}




