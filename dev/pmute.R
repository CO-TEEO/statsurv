pmute <- function(.data, ...) {
  grouping_variables <- groups(.data)
  .data <- dplyr::rowwise(.data, !!!grouping_variables)
  nr <- nrow(.data)

  # Do some mucking around with the ... arguments to make sure that they each argument returns a
  # list
  cls <- enexprs(...)
  ## Set the names correctly
  nms <- names(cls)
  if (any(nms == "")) {
    labels <- map_chr(cls, function(x) deparse1(x))
    no_names <- nms == ""
    names(cls)[no_names] <- labels[no_names]
  }
  ## Wrap each mutate command in the ensure_list_of_one_function
  f2 <- function(x, nm) {
    special_args <- c(".keep", ".before", ".after")
    if (nm %in% special_args) {
      return(eval(x))
    } else {
      return(expr(ensure_list_of_one(!!x)))
    }
  }
  cls2 <- imap(cls, f2)

  # If we want a progress bar, we split, apply the mutate one-at-a-time, and then combind together.
  if (getOption("statsurv.progress", default = TRUE)) {
    # This is what we'd do if we want to have a progress bar
    new_call <- expr(mutate(curr_row, !!!cls2))
    all_rows <- rep(list(NA), nrow(.data))
    split_data <- split(.data, seq_len(nrow(.data)))
    p <- progressr::progressor(along = split_data)
    for (ind in seq_along(split_data)) {
      curr_row <- .data[ind, ]
      all_rows[[ind]] <- eval(new_call)
      p()
    }
    res <- bind_rows(all_rows)
  } else {
    # Otherwise we can just call mutate once.
    new_call <- expr(mutate(.data, !!!cls2))
    res <- eval(new_call)
  }

  # Check all the columns to see if we can un-list them.
  cn <- colnames(res)
  for (nm in cn) {
    res[[nm]] <- check_and_delist(res[[nm]])
  }

  # Reform the original grouping structure
  res <- res %>%
    group_by(!!!grouping_variables)
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
  if (all(map_lgl(col, is_scalar_atomic))) {
    return(unlist(col))
  } else {
    return(col)
  }
}
