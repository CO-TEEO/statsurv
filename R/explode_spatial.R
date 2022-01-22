
collapse_if_exploded <- function(df) {
  n_space <- length(unique(df$id_space))
  if (n_space == 1) {
    return(df)
  }

  # Collapsing these things isn't always straightforward.
  # But the general idea is
  # group_by(id_time)
  # id_space should be the first entry
  # .n_predict should be ... a list of distinct values, or not a list if we don't have to be
  # lists of data.frames become larger data.frames,
  # lists of fits stay as lists of fits
  # lists of nulls become a list of a single null

  inner_collapse <- function(x) {
    # Check if all entries all NULL
    if (all(vapply(x, is.null, logical(1)))) {
      return(list(NULL))
    }

    # Check if all entries are data.frames
    prototype <- x[[1]]
    if (is.data.frame(prototype)) {
      return(list(dplyr::bind_rows(x)))
    }

    # Otherwise just return the objects as a list
    list(x)
  }

  uniq_collapse <- function(x) {
    list(unique(x))
  }

  collapsed_df <- df %>%
    dplyr::group_by(id_time) %>%
    dplyr::summarize(dplyr::across(.fns = inner_collapse))
    # dplyr::summarize(dplyr::across(c(id_space, .n_predict), uniq_collapse),
    #                  dplyr::across(c(-id_space, -.n_predict), .fns = inner_collapse))

  unnest_scalars <- function(x) {
    if (all(vapply(x, function(x) length(x) == 1, logical(1)))) {
      return(unlist(x))
    }
    x
  }
  collapsed_df %>%
    dplyr::mutate(dplyr::across(.fns = unnest_scalars))
    # mutate(dplyr::across(c(id_space, .n_predict), unnest_scalars))
}
