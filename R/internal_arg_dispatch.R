do.call.with.dots <- function(what, ..., list_of_args) {
  # Allows do.call to be called with both arbitrary named arguments and a list of arguments
  args_1 <- list(...)
  args_2 <- list_of_args
  all_args <- c(args_1, args_2)
  do.call(what, all_args)
}



pad_args <- function(arg_list, reference) {
  # Takes a list of arguments, and builds a list of lists that are each the same length.
  # Or, if reference is a gridlist, builds a list of gridlists.
  if (length(arg_list) == 0) {
    padded_args <- reference
    if (is_type(reference, "gridlist")) {
      padded_args[] <- list(list(NULL))
    } else {
      padded_args[] <- list(NULL)
    }
    return(padded_args)
  }

  for (ind in seq_len(length(arg_list))) {
    entry <- arg_list[[ind]]

    if (!identical(class(entry), "list") && !is_type(entry, "gridlist")) {
      # entry is neither a list nor a gridlist
      # Never dispatch, always repeat.
      # Note that I'm using (class(entry) != "list" deliberately - using is.list catches lots of
      # things that aren't actually lists, like data.frames and model fits)
      if (is_type(reference, "gridlist")) {
        new_entry <- reference
        new_entry[] <- list(list(entry))
        arg_list[[ind]] <- new_entry
      } else {
        new_entry <- reference
        new_entry[] <- list(entry)
        arg_list[[ind]] <- new_entry
      }
      next
    }

    if (is_type(entry, "gridlist")) {
      if (!is_type(reference, "gridlist")) {
        stop("gridlist arguments are only valid if the reference object ",
             "is also a gridlist")
      }
      arg_list[[ind]] <- pad_gridlist_to_gridlist(entry, reference)
    } else if (is_type(entry, "list")) {
      if (is_type(reference, "list")) {
        arg_list[[ind]] <- pad_list_to_list(entry, reference)
      } else if (is_type(reference, "gridlist")) {
        arg_list[[ind]] <- pad_list_to_gridlist(entry, reference)
      }
    }

  }
  if (!is_type(reference, "gridlist")) {

    padded_list <- list_transpose(arg_list)
    return(padded_list)
  } else {
    padded_list <- lapply(arg_list, gridcoord::gcl_as_list)
    padded_list <- list_transpose(padded_list)
    padded_list <- lapply(padded_list, list_transpose)
    padded_list <- gridcoord::gcl_gridlist(padded_list,
                                           attr(reference, "space_coord"),
                                           attr(reference, "time_coord"))
    return(padded_list)
  }
}

pad_gridlist_to_gridlist <- function(x, reference) {
  # Name requirements:
  # All the non-NA values in reference are also in x
  # All values in x are in reference
  match_one <- all(rownames(x) %in% rownames(reference)) &&
    all(colnames(x) %in% colnames(reference))

  trimmed_reference <- gridcoord::gcl_gridlist(reference,
                                               attr(reference, "space_coord"),
                                               attr(reference, "time_coord"),
                                               trim = TRUE)
  # We need all the names in the entry to be a subset of the names in reference
  match_two <- all(rownames(trimmed_reference) %in% rownames(x)) &&
    all(colnames(trimmed_reference) %in% colnames(x))



  if (match_one && match_two) {
    return(gridcoord::gcl_gridlist(x,
                                   attr(reference, "space_coord"),
                                   attr(reference, "time_coord")))
  } else {
    stop("gridlists can only be passed as extra arguments if their coordinates match ",
         "those of the reference object. Make sure the coordinate labels of this ",
         "argument match those of the reference, or turn this gridlist into a regular ",
         "list-of-lists to avoid this behavoir")
  }
}


pad_list_to_list <- function(x, reference) {
  # Name requirements in order to dispatch:
  # All values in x are in reference
  # All the non-NA values in reference are also in x

  match_one <- all(names(x) %in% names(reference))
  match_two <- all(names(reference)[!is.na(reference)] %in% names(x))


  if (match_one && match_two) {
    # If both of these are two, then we dispatch
    labels <- names(reference)
    missing_labels <- labels[!labels %in% names(x)]
    x[missing_labels] <- NA
    order <- order(match(names(x), names(reference)))
    return(x[order])
  }

  if (length(x) == length(reference)) {
    warning("Ambiguous whether argument ", x, " should be dispatched",
            " Currently assuming that it should not be dispatched. ",
            "\nSet the names of this argument to the names of the coordinate ",
            "to avoid this behavoir", immediate. = TRUE)
  }
  new_arg <- rep(list(x), time = length(reference))
  names(new_arg) <- names(reference)
  return(new_arg)
}

pad_list_to_gridlist <- function(x, reference) {
  space_coord <- attr(reference, "space_coord")
  time_coord <- attr(reference, "time_coord")
  # I'm assuming all of my gridlists are 2-dimensional
  # It's probably easiest to just try to turn the list into a gridlist directly
  # And then capture if we fail
  result <- tryCatch({
    gridcoord::gcl_gridlist(x, space_coord, time_coord)
  }, error = function(error_condition) {
    NULL
  })
  if (!is.null(result)) {
    return(result)
  }



  # If we can't turn the list into a gridlist directly, then we have
  # 3 options: names match space, names match time, or names don't match anything.
  trimmed_reference <- gridcoord::gcl_gridlist(reference,
                                               attr(reference, "space_coord"),
                                               attr(reference, "time_coord"),
                                               trim = TRUE)

  if (all(names(x) %in% rownames(reference)) &&
      all(rownames(trimmed_reference) %in% names(x))) {
    slices <- rep(list(x), nrow(time_coord))
    names(slices) <- gridcoord::gc_get_labels(time_coord)
    return(gridcoord::gcl_gridlist(slices, space_coord, time_coord))
  } else if (all(names(x) %in% colnames(reference)) &&
             all(colnames(trimmed_reference) %in% names(x))) {
    slices <- rep(list(x), nrow(space_coord))
    names(slices) <- gridcoord::gc_get_labels(space_coord)
    return(gridcoord::gcl_gridlist(slices, space_coord, time_coord))
  } else {
    entry <- x
    slice <- rep(list(entry), nrow(time_coord))
    names(slice) <- gridcoord::gc_get_labels(time_coord)
    slices <- rep(list(slice), nrow(space_coord))
    names(slices) <- gridcoord::gc_get_labels(space_coord)
    return(gridcoord::gcl_gridlist(slices, space_coord, time_coord))
  }
}
