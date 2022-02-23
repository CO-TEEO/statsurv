mat_to_list <- function(mat) {
  # This function is equivalent to as.list(as.data.frame(mat)), but significantly faster
  # https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r#6821395
  lapply(seq_len(ncol(mat)), function(i) mat[, i])
}

