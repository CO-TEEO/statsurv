flatten <- function(x) {
  unname(unlist(x))
}

mat_to_list <- function(mat) {
  # This function is equilvalant to as.list(as.data.frame(mat)), but significantly faster
  # https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r#6821395
  lapply(seq_len(ncol(mat)), function(i) mat[, i])
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

stop_subclass <- function(..., .subclass = NULL) {
  message <- paste0(...)
  err <- structure(
    list(message = message,
         call = NULL),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}



skipping_nulls <- function(f) {
  force(f)
  function(x, ...) {
    if (is.null(x)) {
      return(NULL)
    } else {
      return(f(x, ...))
    }
  }
}


ensure_list <- function(x) {
  stop("Called this function!")
  if (is.data.frame(x) || !is.list(x) ) {
    return(list(x))
  } else {
    return(x)
  }
}
