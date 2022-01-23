#' Apply mutate rowwise
#'
#' A version of mutate that automatically works row-wise and handles list-columns elegantly
#'
#' \code{\link[dplyr]{mutate}} is an extremely powerful function for adding or transforming variables in a data frame. However, it's not designed for working with list-columns, so the syntax is often unwieldy. \code{rowmute} is a wrapper around \code{mutate} that handles that syntax and helps avoid some of the pitfalls.
#'
#' \code{rowmute} differs from \code{mutate} in 3 ways:
#' \enumerate{
#'    \item \code{rowmute} always works one row at a time.
#'    \item \code{rowmute} automatically puts complex output, such as model fit results, into list columns
#'    \item \code{rowmute} can automatically display progress output using the \code{\link[progressr:progressr-package]{progressr}} package
#'    }
#'
#' @param .data A data frame or a data frame extension (e.g. a tibble)
#' @param ... 	<data-masking> Name-value pairs. The name gives the name of the column in the output. Any output other than a vector of length 1 will be stored in a list-column.
#'
#' @return
#' @export
#'
#' @examples
rowmute <- function(.data, ...) {
  grouping_variables <- dplyr::groups(.data)
  .data <- dplyr::rowwise(dplyr::ungroup(.data), !!!grouping_variables)
  nr <- nrow(.data)

  # Do some mucking around with the ... arguments to make sure that they each argument returns a
  # list
  cls <- rlang::enexprs(...)
  ## Set the names correctly
  nms <- names(cls)
  if (any(nms == "")) {
    labels <- purrr::map_chr(cls, function(x) deparse1(x))
    no_names <- nms == ""
    names(cls)[no_names] <- labels[no_names]
  }
  ## Wrap each mutate command in the ensure_list_of_one_function
  f2 <- function(x, nm) {
    special_args <- c(".keep", ".before", ".after")
    if (nm %in% special_args) {
      return(eval(x))
    } else {
      return(rlang::expr(ensure_list_of_one(!!x)))
    }
  }
  cls2 <- purrr::imap(cls, f2)

  # If we want a progress bar, we split, apply the mutate one-at-a-time, and then combind together.
  if (getOption("statsurv.progress", default = TRUE)) {
    # This is what we'd do if we want to have a progress bar
    new_call <- rlang::expr(dplyr::mutate(curr_row, !!!cls2))
    all_rows <- rep(list(NA), nrow(.data))
    split_data <- split(.data, seq_len(nrow(.data)))
    p <- progressr::progressor(along = split_data)
    for (ind in seq_along(split_data)) {
      curr_row <- .data[ind, ]
      all_rows[[ind]] <- eval(new_call)
      p()
    }
    res <- dplyr::bind_rows(all_rows)
  } else {
    # Otherwise we can just call mutate once.
    new_call <- rlang::expr(dplyr::mutate(.data, !!!cls2))
    res <- eval(new_call)
  }

  # Check all the columns to see if we can un-list them.
  cn <- colnames(res)
  for (nm in cn) {
    res[[nm]] <- check_and_delist(res[[nm]])
  }

  # Reform the original grouping structure
  res <- res %>%
    dplyr::group_by(!!!grouping_variables)
  return(res)
}


ensure_list_of_one <- function(x) {
  if (!is.list(x) || length(x) != 1) {
    return(list(x))
  } else {
    return(x)
  }
}

check_and_delist <- function(col) {
  if (!is.list(col)) {
    return(col)
  }
  if (all(purrr::map_lgl(col, rlang::is_scalar_atomic))) {
    return(vctrs::vec_c(!!!col))
    # Using the vctrs version to try to preserve type information during unlisting.
    # return(unlist(col))
  } else {
    return(col)
  }
}
