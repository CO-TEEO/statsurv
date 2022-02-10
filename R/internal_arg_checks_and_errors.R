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
  stop("Called function ensure_list in internal_arg_checks_and_errors")
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






validate_zones <- function(zones, spacetime_data) {
  if (!is.list(zones)) {
    stop("zones must be a list")
  }
  if (!all(unique(unlist(zones)) %in% spacetime_data$id_space)) {
    stop("All entries in ones correspond to values in data$id_space")
  }
  invisible(zones)
}

is_scalar_numeric <- function(x) {
  rlang::is_scalar_double(x) || rlang::is_scalar_integer(x)
}
