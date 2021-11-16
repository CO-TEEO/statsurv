flip_df <- function(df) {
  df[seq(nrow(df), 1, by = -1), , drop = FALSE]
}

simple_cumsum <- function(wide_x) {
  if (is.vector(wide_x)) {
    return(matrix(wide_x, nrow = 1))
  }
  if (nrow(wide_x) == 1) {
    return(wide_x)
  }
  return(apply(wide_x, 2, cumsum))
}
