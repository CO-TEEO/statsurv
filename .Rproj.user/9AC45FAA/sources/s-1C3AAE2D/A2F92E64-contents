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
loop_model <- function(space_coord,
                       time_coord,
                       data_for_model,
                       outcome_col,
                       path_to_model,
                       min_train = 7,
                       max_train = Inf,
                       n_predict = 1,
                       model_arity = c("multi", "uni"),
                       use_cache = FALSE,
                       force = FALSE,
                       save_environments = FALSE,
                       verbose = interactive(),
                       extra_model_args = list()) {

  ### Helper functions ----
  wrap_data <- function(f) {
    # NA's out the appropriate data, calls the model function, then patches the data
    # back together.
    force(f)
    function(space_coord, time_coord, data_for_model, extra_model_args, .mask, .outcome_col) {

      na_data_for_model <- data_for_model
      na_data_for_model[.mask, .outcome_col] <- NA

      ret_val <- do.call.with.dots(f,
                                   space_coord,
                                   time_coord,
                                   na_data_for_model,
                                   list_of_args = extra_model_args)
      output_data <- ret_val[[2]]
      output_data[.mask, .outcome_col] <- data_for_model[.mask, .outcome_col]
      ret_val[[2]] <- output_data
      return(ret_val)
    }
  }


  ### Argument checks ----
  space_coord <- gridcoord::gc_gridcoord(space_coord)
  time_coord <- gridcoord::gc_gridcoord(time_coord)
  check_type(data_for_model, "data.frame")
  check_scalar_type(outcome_col, "character")
  check_scalar_type(path_to_model, c("character", "function"))
  check_scalar_type(min_train, "integer")
  check_scalar_type(max_train, "integer")
  if (min_train < 1) {
    stop_subclass("min_train must be a positive integer", .subclass = "error_bad_arg_value")
  }
  if (min_train >= nrow(time_coord)) {
    stop_subclass("min_train is greater than or equal to the number of distinct time points. ",
                       "Unable to calculate any model fits.", .subclass = "error_bad_arg_value")
  }
  if (max_train < min_train) {
    stop_subclass("max_train must be greater than or equal to min_train",
                  .subclass = "error_bad_arg_value")
  }
  check_scalar_type(n_predict, "integer")
  model_arity <- match.arg(model_arity)
  check_scalar_type(use_cache, "logical")
  # Force and save_environments passed through to cache_wrap

  # Extra checks that data_for_model and our coordinates line up
  data_for_model <- match_coords_and_data(space_coord, time_coord, data_for_model)


  ### Transform into gridlists ----
  if (model_arity == "uni" && is_multiariate(space_coord, data_for_model)) {
    # Split the data spatially
    # This is required if the model can only handle one output stream (e.g. some ARIMA models), but
    # the data has multiple spatial areas.
    data_for_model <- gridcoord::gcl_gridlist(data_for_model, space_coord)
    space_coord <- space_coord_split(space_coord)
    total_space <- nrow(data_for_model)
  } else {
    total_space <- 1
  }


  ### Load the model function ----
  if (is.function(path_to_model)) {
    model_function <- path_to_model
    path_to_model <- fix_up_path(deparse(substitute(path_to_model)))

  } else if (is.character(path_to_model)) {
    model_function <- magic_function_loader(path_to_model)
  }



  ### Set up caching ----
  # Wrap the function so we have the fit and data together
  wrapped_model_function <- wrap_data(model_function)
  if (use_cache) {
    cache_dir <- file.path("cache_model_fits",
                           tools::file_path_sans_ext(basename(path_to_model)))
  } else {
    cache_dir <- NULL
  }
  cached_wrapped_model_function <- simplecache::cache_wrap(wrapped_model_function,
                                                           cache_dir,
                                                           hash_in_name = FALSE,
                                                           save_environments = save_environments)
  force <- parse_force(force, time_coord)

  ### Create progress bar
  progress_bar <- dot_progress_functional(total = total_space,
                                          dot_every = 1,
                                          number_every = 5,
                                          title = "Calculating model fits",
                                          verbose = verbose)


  ### Loop over spatial regions ----
  if (is_type(data_for_model, "gridlist")) {
    return_accum <- list()
    for (space_ind in seq_len(nrow(data_for_model))) {
      curr_data <- gridcoord::gcl_collapse(data_for_model[space_ind, , drop = FALSE],
                                           collapse_by = "space")[[1, 1]]

      curr_space_coord <- space_coord[[space_ind]]
      curr_space_name <- gridcoord::gc_get_labels(curr_space_coord)[[1]]
      return_accum[[curr_space_name]] <-
        loop_model_int(curr_space_coord, time_coord, curr_data, outcome_col,
                       cached_wrapped_model_function,
                       force,
                       min_train, max_train, n_predict,
                       extra_model_args,
                       progress_bar)


    }
    # After this loop, we're organized as space - x/y/z - time
    # loop transpose to get x/y/z - space - time
    # Then turn each element in a gridlist and return.
    rets <- lapply(list_transpose(return_accum),
                   gridcoord::gcl_gridlist,
                   space_coord = do.call(rbind, space_coord),
                   time_coord = time_coord)
    return(rets)
  } else {
    rets <- loop_model_int(space_coord, time_coord, data_for_model, outcome_col,
                   cached_wrapped_model_function,
                   force,
                   min_train, max_train, n_predict,
                   extra_model_args,
                   progress_bar)
    return(rets)
  }
}

loop_model_int <- function(space_coord,
                           time_coord,
                           data_for_model,
                           outcome_col,
                           model_function,
                           force,
                           min_train,
                           max_train,
                           n_predict,
                           extra_model_args,
                           progress_bar) {

  # The internal function just takes care of looping over end dates

  tc_name <- gridcoord::gc_get_name(time_coord)
  all_returns <- list()
  last_training_tinds <- seq(min_train, nrow(time_coord) - 1, by = n_predict)
  total_time <- length(last_training_tinds)

  for (ind in last_training_tinds) {
    start_ind <- max(ind - max_train + 1, 1)
    fin_ind <- min(ind + n_predict, nrow(time_coord))
    prediction_inds <- seq(ind + 1, fin_ind, by = 1)
    curr_time_label <- as.character(gridcoord::gc_get_labels(time_coord)[[fin_ind]])
    prediction_labels <- gridcoord::gc_get_labels(time_coord)[prediction_inds]

    cache_name <- build_cache_name(curr_time_label, space_coord)


    # Subset the data by time
    limited_time_coord <- time_coord[start_ind:fin_ind, , drop = FALSE]
    limited_data <- gridcoord::gc_pare(data_for_model, limited_time_coord)
    m <- limited_data[[tc_name]] %in% prediction_labels
    fit_and_data <- model_function(space_coord,
                                   limited_time_coord,
                                   limited_data,
                                   extra_model_args,
                                   .name_prefix = cache_name,
                                   .force = force[[curr_time_label]],
                                   .mask = m,
                                   .outcome_col = outcome_col)
    # Accumulate the result, update progress bar
    all_returns[[curr_time_label]] <- fit_and_data
    progress_bar(step = 1 / total_time)
  }

  # Transpose the lists and pad with NAs
  fits_and_data <- list_transpose(all_returns)
  padded_fits_and_data <- lapply(fits_and_data, pad_with_nas, coord = time_coord)
  return(padded_fits_and_data)
}


