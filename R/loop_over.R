#' @title Iterate over lists and gridlists
#'
#' @description Like \code{\link[base]{lapply}} or \code{\link[purrr]{map}}, `loop_over` provides a
#'   flexible way to iterate over a list of objects. `loop_over` is designed to work with objects
#'   organized either by time or by both space and time, making it more specialized than these other
#'   options.  `loop_over` includes support for \code{\link[gridcoord:gcl_gridlist]{gridlists}},
#'   two-dimensional lists for organizing information in both space and time, provides flexible
#'   tools for dispatching arguments, and has support for caching built in to it.
#'
#' @param space_coord A gridcoord object
#'   (\code{\link[gridcoord]{gc_gridcoord}}) describing the spatial area that is
#'   covered by `list_of_stuff`
#' @param time_coord A gridcoord object (\code{\link[gridcoord]{gc_gridcoord}})
#'   describing the temporal area that is to be covered by the model. This coordinate must be
#'   ordered, with the first entries in the dataframe corresponding to the earliest time periods and
#'   the last entries corresponding to the most recent.
#' @param list_of_stuff Either a list or a \code{\link[gridcoord:gcl_gridlist]{gridlists}} generated
#'   by one of the other looping functions in `statsurv`. The names of the entries in the list or
#'   gridlist must correspond to the values in `time_coord` if `list_of_stuff` is a list or to the
#'   values in both `time_coord` and `space_coord` if `list_of_stuff` is a
#'   \code{\link[gridcoord:gcl_gridlist]{gridlist}}
#' @param f The function to apply to each element
#' @param verbose Should updates be printed to the console?
#' @param title If `verbose = TRUE`, the title of the progress bar
#' @param use_cache Should the results from this function be saved (cached), so that the results can
#'   be quickly loaded without having to re-run the calculation?
#' @param path_to_model The path to the model function, or the function itself, used to calculate
#'   the model fits in \code{\link{loop_model}}. Only used in setting up the cache.
#' @param force If previous results have been cached, should the function be forced to redo the
#'   calculations? \code{force} can be a single logical value or a vector indicating specific time
#'   points to be re-calculated.
#' @param extra_args A named list containing additional parameters to supply to the
#'   extractor function.
#'
#' @return A list or a \code{\link[gridcoord:gcl_gridlist]{gridlist}} with the same length or
#'   dimensions as `list_of_stuff`. Each entry in the list or
#'   \code{\link[gridcoord:gcl_gridlist]{gridlist}} is the result of the function `f` applied to the
#'   corresponding entry in `list_of_stuff`.
#'
#' @details
#'   Additional arguments can be passed to `f` by including them as entries in
#'   the `extra_args` parameter. All the components in `extra_args` will be passed
#'   through unchanged to `f` **unless** any of the components are lists with
#'   the same names as the `list_of_stuff` argument. In this case, each time the extrctor
#'   function is run, `loop_over` will pass a different element of the component into `f`
#'
#' @inheritSection loop_model Caching overview
#' @examples
#' space_coord <- gridcoord::gc_stubcoord()
#' time_coord <- data.frame(time = c("t1", "t2", "t3"),
#'                          stringsAsFactors = FALSE)
#' simple_list <- list(t1 = c(1, 2), t2 = c(2, 4), t3 = c(5, NA))
#'
#' # Calculate the mean of every entry in a list:
#' loop_over(space_coord, time_coord, simple_list, mean)
#'
#' # Calculate the mean of every entry in a list,
#' # with na.rm = TRUE
#' loop_over(space_coord, time_coord, simple_list, mean,
#'           extra_args = list(na.rm = TRUE))
#'
#' # Multiply each entry in a list by the entry in another list:
#' loop_over(space_coord, time_coord, simple_list, function(x, b) x * b,
#'           extra_args = list(b = list(t1 = 3, t2 = 4, t3 = 0)))
#'
#' list_s1 <- list("t1" = 1, "t2" = 2, "t3" = 3)
#' list_s2 <- list("t1" = 10, "t2" = 20, "t3" = 30)
#' space_coord <- data.frame(space = c("space1", "space2"),
#'                           stringsAsFactors = FALSE)
#' st_list <- gridcoord::gcl_gridlist(list(space1 = list_s1,
#'                                         space2 = list_s2),
#'                                    space_coord,
#'                                    time_coord)
#'
#' # Multiply each entry in a gridlist by 2:
#' loop_over(space_coord, time_coord, st_list, function(x) x*2)
#'
#' @family looping functions
#' @export
#' @md
loop_over <- function(space_coord,
                      time_coord,
                      list_of_stuff,
                      f,
                      verbose = interactive(),
                      title = NULL,
                      use_cache = FALSE,
                      path_to_model = NULL,
                      force = FALSE,
                      extra_args = list()) {

  if (is_type(list_of_stuff, "gridlist")) {
    total_space <- nrow(list_of_stuff)
    space_coord <- space_coord_split(space_coord)
  } else {
    total_space <- 1
  }

  # 1. Deal with the extra arguments
  list_args <- pad_args(extra_args, reference = list_of_stuff)

  # Set up caching
  f_path <- fix_up_path(deparse(substitute(f)))
  cache_dir <- calc_cache_dir(use_cache,
                              top_level = "cache_loop_over",
                              deparse(substitute(path_to_model)),
                              tools::file_path_sans_ext(basename(f_path)))


  cached_f <- simplecache::cache_wrap(f,
                                      cache_dir,
                                      hash_in_name = FALSE,
                                      save_environments = TRUE)

  force <- parse_force(force, names(list_of_stuff))



  progress_bar <- dot_progress_functional(total = total_space,
                                          dot_every = 1,
                                          number_every = 5,
                                          title = title,
                                          verbose = verbose)

  ### Loop over spatial regions ----
  if (is_type(list_of_stuff, "gridlist")) {
    return_accum <- list()
    for (space_ind in seq_len(nrow(list_of_stuff))) {
      curr_space_coord <- space_coord[[space_ind]]
      curr_space_name <- gridcoord::gc_get_labels(curr_space_coord)[[1]]


      curr_stuff <- extract_gcl_row(list_of_stuff, space_ind)
      curr_extra_args <-  extract_gcl_row(list_args, space_ind)

      return_accum[[curr_space_name]] <-
        loop_over_int(curr_space_coord,
                      time_coord,
                      curr_stuff,
                      cached_f,
                      force = force,
                      list_of_extra_args = curr_extra_args,
                      progress_bar = progress_bar)
    }
    # currently organized as just a list of lists
    res <- gridcoord::gcl_gridlist(return_accum,
                                   do.call(rbind, space_coord),
                                   time_coord)
    return(res)
  } else {
    res <- loop_over_int(space_coord,
                         time_coord,
                         list_of_stuff,
                         cached_f,
                         force = force,
                         list_of_extra_args = list_args,
                         progress_bar = progress_bar)
    return(res)
  }

}

loop_over_int <- function(space_coord, time_coord, list_of_stuff, f,
                          force, list_of_extra_args, progress_bar) {


  total_time <- sum(!is_singular_na(list_of_stuff))
  n_stuff <- length(list_of_stuff)

  list_of_results <- list()

  for (ind in seq_len(n_stuff)) {
    stuff <- list_of_stuff[[ind]]
    if (identical(stuff, NA)) {
      next
    }

    curr_name <- names(list_of_stuff)[[ind]]


    result <- do.call.with.dots(f,
                                list_of_stuff[[ind]],
                                list_of_args = list_of_extra_args[[ind]])

    # 3. Accumulate the result, update our progress bar
    list_of_results[[curr_name]] <- result
    progress_bar(step = 1 / total_time)
  }

  padded_results <- pad_with_nas(list_of_results, time_coord)
  return(padded_results)
}
# At the moment, this is just a lapply/purrr::map clone.
# But I think making it our own function is probably useful.
# B/c we can:
# 1. Add caching
# 2. Add support for space explosions
# 3. Better coordinate support.
