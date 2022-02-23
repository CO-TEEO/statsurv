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
