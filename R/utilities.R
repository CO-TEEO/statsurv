flatten <- function(x) {
  unname(unlist(x))
}

magic_function_loader <- function(.filename) {
  # A tricky function that sources a file with a single function in it, and returns that function
  check_type(.filename, "character")
  source(.filename, local = TRUE)
  func_names <- names(environment())
  func_names <- func_names[func_names != ".filename"]
  if (length(func_names) > 1) {
    stop("magic_function_loaded can only be used to source a file with a single function in it")
  }
  func <- get(func_names[[1]], inherits = FALSE)
  if (!is.function(func)) {
    stop(deparse(substitute(filename)), " must contain a function")
  }
  return(func)
}

dot_progress_functional <- function(total, dot_every, number_every, title = NULL, verbose = TRUE) {
  # Prints a progress bar to the screen - with a dot every dot_every percent, and a number every
  # number_every percent
  if (!verbose) {
    return(invisible(function(step = 0) {}))
  }
  i <- 0
  prev_pcnt <- 0
  if (!is.null(title)) {
    message(title)
  }
  message("0", appendLF = FALSE)
  function(step = 1) {
    i <<- i + step
    new_pcnt <- floor(i * 100 / total)
    if (new_pcnt != prev_pcnt) {
      for (pcnt in seq(prev_pcnt + 1, new_pcnt)) {
        if (pcnt %% number_every == 0) {
          message(pcnt, appendLF = FALSE)
        } else if (pcnt %% dot_every == 0) {
          message(".", appendLF = FALSE)
        }
        if (signif(pcnt, 5) == 100) {
          message("")
        }
      }
    }
    prev_pcnt <<- new_pcnt
  }
}


mat_to_list <- function(mat) {
  # This function is equilvalant to as.list(as.data.frame(mat)), but significantly faster
  # https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r#6821395
  lapply(seq_len(ncol(mat)), function(i) mat[, i])
}

stack_df <- function(list_of_dfs) {
  # Takes a list of data frames or matrices and stacks them together to form a 3-dimensional array
  df1 <- list_of_dfs[[1]]
  n <- length(list_of_dfs)
  new_dims <- c(dim(df1), n)
  new_array <- array(dim = new_dims)
  for (ind in seq_len(length(list_of_dfs))) {
    new_array[, , ind] <- as.matrix(list_of_dfs[[ind]])
  }
  return(new_array)
}

stack_vectors <- function(list_of_vectors) {
  v1 <- list_of_vectors[[1]]
  n <- length(list_of_vectors)
  new_dims <- c(length(v1), n)
  new_array <- array(dim = new_dims)
  for (ind in seq_len(length(list_of_vectors))) {
    new_array[, ind] <- as.array(list_of_vectors[[ind]])
  }
  return(new_array)
}

stack_and_average <- function(list_of_stuff) {
  first <- list_of_stuff[[1]]
  if (is.vector(first)) {
    averaged <- stack_vectors(list_of_stuff) %>%
      rowMeans(dims = 1)
    names(averaged) <- names(first)
  } else if (is.data.frame(first) || is.matrix(first)) {
    averaged <- stack_df(list_of_stuff) %>%
      rowMeans(dims = 2)
    if (is.data.frame(first)) {
      averaged <- as.data.frame(averaged)
    }
    rownames(averaged) <- rownames(first)
    colnames(averaged) <- colnames(first)
  }
  return(averaged)
}


calc_cache_dir <- function(use_cache, top_level, path_to_model, ...) {
  if (!use_cache) {
    return(NULL)
  }

  if (is.null(path_to_model)) {
    cache_dir <- file.path(top_level,
                           ...)
    return(cache_dir)
  }

  clean_path <- parse_path(path_to_model)
  if (clean_path == "tempdir") {
    return(tempdir())
  } else {
    cache_dir <- file.path(top_level,
                           tools::file_path_sans_ext(basename(clean_path)),
                           ...)
    return(cache_dir)
  }
  stop("Unable to determine cache directory")
}

list_transpose <- function(list_of_lists) {
  outer_names <- names(list_of_lists)
  inner_names <- names(list_of_lists[[1]])
  n_outer <- length(list_of_lists)
  n_inner <- length(list_of_lists[[1]])
  new_list <- list(vector("list", n_outer)) %>% rep(n_inner)
  for (ind_out in seq_len(n_outer)) {
    for (ind_in in seq_len(n_inner)) {
      new_list[[ind_in]][ind_out] <- list_of_lists[[ind_out]][ind_in]
    }
  }
  names(new_list) <- inner_names
  for (ind in seq_along(new_list)) {
    names(new_list[[ind]]) <- outer_names
  }
  return(new_list)
}

transfer_to_file <- function(in_f, output_filename = tempfile()) {
  bod <- attr(in_f, "srcref") %>% as.character() %>% paste(collapse = "\n")
  nam <- paste0(deparse(substitute(in_f)), " <- ")
  cat(c(nam, bod), file = output_filename)
  return(output_filename)
}


fix_up_path <- function(path_to_model) {
  check_type(path_to_model, "character")
  if (substr(path_to_model[[1]], 1, 9) == "function(") {
    collapsed_path <- make.names(paste0(path_to_model, collapse = ""))
    subbed_path <- gsub(pattern = "\\.", replacement = "", x = collapsed_path)
    trimmed_path <- substr(subbed_path, 1, 32)
    return(trimmed_path)
  } else {
    return(path_to_model)
  }

}

parse_path <- function(path_to_model) {
  check_type(path_to_model, c("character", "function"))
    if (is.function(path_to_model)) {
      path_to_model <- deparse(substitute(path_to_model))
    }
    return(fix_up_path(path_to_model))
}



mc_pvalue <- function(observed, replicates) {
  if (length(replicates) == 0) {
    return(NULL)
  }
  else {
    f <- Vectorize(function(y) {
      (1 + sum(replicates > y)) / (1 + length(replicates))
    })
    return(f(observed))
  }
}


pluck_out <- function(item, ...) {
  args <- list(...)
  for (element in args) {
    item <- item[[element]]
  }
  return(item)
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


parse_force <- function(force, labels = NULL) {

  no_parse_error <- function() {
    stop("Unable to parse 'force'.",
         " force can be specified as an index into labels (logical or numeric)",
         " or as a vector of coordinate labels to use")
  }
  # Force can be:
  # 1. A single T/F value
  # 2. A logical vector of length equal to labels
  # 3. A vector of labels
  # 4. A numeric vector? Or is that just asking for trouble?

  # labels can be:
  # 1. A vector of length equal to force
  # 2. A dataframe, in which case the first column is used as the labels

  # And we're going to return:
  # A named logical mask of length nrow(labels)
  if (is.data.frame(labels)) {
    labels <- labels[[1]]
  }
  n <- length(labels)
  # Throw an error if:
  #   It's valid to interpret force as both a list of labels, or as a regular index
  match_interpretation <- match(force, labels)
  valid_match <- !any(is.na(match_interpretation))

  valid_numbers <- is.numeric(force) && all(abs(force) <= n) && all(sign(force) == sign(force[[1]]))
  valid_logical <- is.logical(force) && (length(force) == 1 || length(force) == n)
  valid_index <- valid_numbers || valid_logical
  if (valid_match && valid_index) {
    stop("Unable to unambiguously parse force. This is often caused by using logical or ",
         "integer values as the labels in your coordinate data frame")
  }
  if (!(valid_match || valid_index)) {
    no_parse_error()
  }

  if (valid_match) {
    std_force <- rep(FALSE, n)
    std_force[match_interpretation] <- TRUE
    names(std_force) <- labels
    return(std_force)
  }

  if (is.logical(force)) {
    if (length(force) == 1) {
      std_force <- rep(force, n)
      names(std_force) <- labels
      return(std_force)
    } else if (length(force) == n) {
      std_force <- force
      names(std_force) <- labels
      return(std_force)
    } else {
      # We shouldn't ever be able to get here, but we'll keep it in anyway, just in case
      stop("Invalid value of 'force'. If force is logical, it must either be a scalar or a ",
           "vector with the same length as the number of rows in labels")
    }
  }

  if (is.numeric(force)) {
    std_force <- rep(FALSE, n)
    std_force[force] <- TRUE
    names(std_force) <- labels
    return(std_force)
  }

  # We shouldn't ever be able to get here, but we'll keep it in anyway, just in case
  no_parse_error()
}

space_coord_into_gridlist <- function(x,
                               space_coord,
                               time_coord) {
  # We're going to split, repeat, and collate
  if (gridcoord::gc_get_name(space_coord) == "stubcoord") {
    split_x <- list(x)
    names(split_x) <- gridcoord::gc_get_labels(space_coord)
  } else {
    sc_name <- gridcoord::gc_get_name(space_coord)
    if (is_type(x, "Spatial")) {
      split_x <- sp::split(x, x[[sc_name]])
    } else {
      split_x <- split(x, x[[sc_name]])
    }
  }

  slices <- rep(list(split_x), nrow(time_coord))
  names(slices) <- gridcoord::gc_get_labels(time_coord)
  return(gridcoord::gcl_gridlist(slices, space_coord, time_coord))
}

space_coord_split <- function(space_coord) {
  sc_name <- gridcoord::gc_get_name(space_coord)
  if (is_type(space_coord, "Spatial")) {
    split_space_coord <- sp::split(space_coord, space_coord[[sc_name]])
  } else {
    split_space_coord <- split(space_coord, space_coord[[sc_name]])
  }
  return(split_space_coord)
}

pad_with_nas <- function(x, coord) {
  labels <- as.character(gridcoord::gc_get_labels(coord))
  missing_labels <- labels[!labels %in% names(x)]
  x[missing_labels] <- NA
  order <- order(match(names(x), labels))
  return(x[order])
}

is_singular_na <- function(x) {
  vapply(x, function(x) length(x) == 1 && is.na(x), logical(1))
}

extract_gcl_row <- function(gridlist, i) {
  list_transpose(gridcoord::gcl_as_list(gridlist))[[i]]
}

build_cache_name <- function(curr_time_label, space_coord)  {
  if (!is_multiariate(space_coord)) {
    # For caching, we need a name. We use the heuristic that if space_coord is a single location,
    # include the label in the name. Otherwise include the name in the name.
    curr_space_label <- gridcoord::gc_get_labels(space_coord)[[1]]
    curr_name <- paste0(curr_space_label,
                        "_",
                        curr_time_label)
  } else {
    space_name <- gridcoord::gc_get_name(space_coord)
    curr_name <- paste0(space_name,
                        "_",
                        curr_time_label)
  }
  return(curr_name)
}
