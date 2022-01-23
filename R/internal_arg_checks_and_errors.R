error_type <- function(x, desired, x_name) {
  if (missing(x_name)) {
    x_name <- deparse(substitute(x))
  }
  stop_subclass("\nInvalid type for input: ", x_name,
                "\nRequired type: ", paste0(desired, collapse = ", "),
                "\nActual type: ", paste0(class(x), collapse = ", "),
                .subclass = "error_type")
}

error_scalar <- function(x, x_name) {
  if (missing(x_name)) {
    x_name <- deparse(substitute(x))
  }
  stop_subclass("\n",
                x_name,
                " must be a scalar",
                .subclass = "error_scalar")
}

check_type <- function(x, desired_type, throw_error = TRUE) {
  if (length(desired_type) > 1) {
    all_valid <- vapply(desired_type, check_type, logical(1), x = x, throw_error = FALSE)
    valid <- any(all_valid)
  } else {
    valid <- switch(desired_type,
                    "numeric" = is.numeric(x),
                    "character" = is.character(x),
                    "data.frame" = ,
                    "dataframe" = is.data.frame(x),
                    "integer" = {is.numeric(x) && x == round(x)},
                    "call" = is.call(x),
                    "function" = is.function(x),
                    "NULL" = ,
                    "null" = is.null(x),
                    NULL)

    if (is.null(valid)) {
      if (isS4(x)) {
        valid <- desired_type %in% methods::is(x)
      } else {
        valid <- desired_type %in% class(x)
      }
    }
  }
  if ((!valid) && throw_error) {
    error_type(x, desired_type, deparse(substitute(x)))
  }
  return(valid)
}

check_scalar <- function(x, throw_error = TRUE) {
  # This is actually surprising difficult to define or to check in R
  # (e.g., is a data.frame a scalar?)
  # We'll use length for now, but this fails to things like data.frames or defined classes
  valid <- length(x) == 1
  if ((!valid) && throw_error) {
    error_scalar(x, deparse(substitute(x)))
  }
  return(valid)
}

check_scalar_type <- function(x, desired_type, throw_error = TRUE) {
  valid1 <- check_type(x, desired_type, throw_error = FALSE)
  valid2 <- check_scalar(x, throw_error = FALSE)
  x_name <- deparse(substitute(x))
  if (!valid1 && throw_error) {
    error_type(x, desired_type, x_name)
  }
  if (!valid2 && throw_error) {
    error_scalar(x, x_name)
  }
  return(valid1 && valid2)
}

is_type <- function(x, desired_type) {
  check_type(x, desired_type, FALSE)
}


ensure_list <- function(x) {
  if (is.data.frame(x) || !is.list(x) ) {
    return(list(x))
  } else {
    return(x)
  }
}

stop_subclass <- function(..., .subclass = NULL) {
  message <- paste0(...)
  err <- structure(
    list(message = message,
         call = NULL),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}


match_coords_and_data <- function(space_coord, time_coord, df, subset_ok = FALSE) {
  tc_name <- gridcoord::gc_get_name(time_coord)
  sc_name <- gridcoord::gc_get_name(space_coord)
  df_name <- deparse(substitute(df))
  if (any(!c(tc_name, sc_name) %in% colnames(df))) {
    stop_subclass(df_name, " must contain the columns ", tc_name, " and ",
                  sc_name, ". Update either ", df_name, " or space_coord/time_coord",
                  .subclass = "error_bad_arg_value")
  }
  pared_df <- df %>%
    gridcoord::gc_pare(space_coord) %>%
    gridcoord::gc_pare(time_coord)
  if (!isTRUE(all.equal(pared_df,
                        df,
                        check.attributes = FALSE))) {
    warning("Some rows in ", df_name, " were removed because they were not identified with ",
            " your space and time coordinates")
    df <- pared_df
  }
  if (subset_ok) { # subset_ok mean it's ok for df to have just a subset of the space/time coords
    return(invisible(df))
  }

  # Then check the other way - that every space and time coordinate has at least one row in
  # data_for_model
  combined_coordinates <- gridcoord::gc_expand(time_coord, space_coord)
  coordinates_not_in_data <- dplyr::anti_join(combined_coordinates,
                                              df,
                                              by = c(sc_name, tc_name))
  if (nrow(coordinates_not_in_data) > 0) {
    stop_subclass("Some combinations of coordinates have no corresponding rows in ", df_name, ".",
                  " Use gridcoord::gc_pad to fix this issue",
                  .subclass = "error_bad_arg_value")
  }
  return(invisible(df))
}


validate_data_for_yhat <- function(space_coord, time_coord, data_for_model, data_name) {
  # Our requirements for yhat:
  # The time labels must be contiguous subset of the labels in time_coord
  # Each space label should be associated with the same set of time labels
  tc_name <- gridcoord::gc_get_name(time_coord)
  sc_name <- gridcoord::gc_get_name(space_coord)
  if (any(!c(tc_name, sc_name) %in% colnames(data_for_model))) {
    stop_subclass(data_name, " must contain the columns ", tc_name, " and ",
                  sc_name, ". Update either ", data_name, " or space_coord/time_coord",
                  .subclass = "error_bad_data_yhat")
  }

  t_labels <- unique(data_for_model[[tc_name]])
  if (!all(t_labels %in% time_coord[[tc_name]])) {
    stop_subclass("\nSome of the time points in ", data_name,
                  " are not included in time_coord",
                  .subclass = "error_bad_data_yhat")
  }

  t_inds <- match(t_labels, time_coord[[tc_name]])
  time_inds_ok <- identical(sort(t_inds), seq(min(t_inds), max(t_inds)))
  if (!time_inds_ok) {
    stop_subclass("\n The data.frame ", data_name,
                  " passed to loop_extract_yhat does not span a contiguous subset of the ",
                  "time coordinate. Fix either ", data_name, " or time_coord",
                  "to fix this problem",
                  .subclass = "error_bad_data_yhat")
  }


  data_split_by_space <- split(data_for_model, data_for_model[[sc_name]])
  first_set <- data_split_by_space[[1]][[tc_name]]
  times_the_same <- vapply(data_split_by_space,
                           function(x) setequal(x[[tc_name]],
                                                first_set),
                           logical(1))
  space_ok <- all(times_the_same)
  if (!space_ok) {
    stop_subclass("\n The data.frame ", data_name,
                  " passed to loop_extract_yhat has different time coordinates associated with ",
                  "each space coordinate.  Fix either ", data_name, " or space_coord",
                  "to remove this error",
                  .subclass = "error_bad_data_yhat")
  }
  if (!all(data_for_model[[sc_name]] %in% space_coord[[sc_name]])) {
    stop_subclass("\nSome of the space points in ", data_name,
                  "are not included in space_coord",
                  .subclass = "error_bad_data_yhat")
  }
  return(invisible(data_for_model))
}

throw_model_list_err <- function(list_name) {
  stop_subclass(list_name, " must be a named list, ",
                "where all the names are entries in time_coord",
                .subclass = "error_bad_arg_value")
}

validate_model_list <- function(model_list, time_coord) {
  nms <- names(model_list)
  if (is.null(nms)) {
    throw_model_list_err(deparse(substitute(model_list)))
  }
  if (!all(nms %in% gridcoord::gc_get_labels(time_coord))) {
    throw_model_list_err(deparse(substitute(model_list)))
  }
}

validate_spacetime_data <- function(df) {
  # This is basically the definition of the spacetime dataframe.
  # Requirements:
  # Has the columns id_time and id_space
  # Each is integers or equivalent
  # Each has the values seq(min, max, by = 1)
  if (!"id_time" %in% colnames(df) || !"id_space" %in% colnames(df)) {
    stop_subclass("dataframes used in statsurv must include the columns 'id_time' and 'id_space'",
                  .subclass = "error_bad_spacetimedata")
  }

  if (!rlang::is_integerish(df$id_time, finite = TRUE)) {
    stop_subclass("The column 'id_time' must be made of whole numbers with no gaps or skips",
                  .subclass = "error_bad_spacetimedata")

  }
  if (!rlang::is_integerish(df$id_space, finite = TRUE)) {
    stop_subclass("The column 'id_space' must be made of whole numbers with no gaps or skips",
                  .subclass = "error_bad_spacetimedata")

  }
  time_uniq_val <- sort(unique(df$id_time))
  if (!all.equal(time_uniq_val, seq(min(df$id_time), max(df$id_time), by = 1))) {
    stop_subclass("The column 'id_time' must be made of whole numbers with no gaps or skips",
                  .subclass = "error_bad_spacetimedata")
  }

  space_uniq_val <- sort(unique(df$id_space))
  if (!all.equal(space_uniq_val, seq(min(df$id_space), max(df$id_space), by = 1))) {
    stop_subclass("The column 'id_space' must be made of whole numbers with no gaps or skips",
                  .subclass = "error_bad_spacetimedata")
  }
  return(invisible(df))
}
