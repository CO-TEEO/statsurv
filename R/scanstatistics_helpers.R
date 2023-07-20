#' @title Change how spatial zones are represented
#'
#' @description Scan-type alarm functions work by computing an alarm function over a very large
#'   number of spatial *zones*, where each zone consists of one of more neighboring spatial
#'   locations. Information about which spatial locations belong to which zone can be specified in
#'   the form of a list, where each entry in the list contains the indexes of the locations in a
#'   particular zone, or in the form of a matrix, where each row represents a zone and each column
#'   represents a location. A set of zones can be generated using the
#'   \code{\link{create_zones}} function, and conversion between the list- and
#'   matrix-representation of the zones is accomplished through the functions
#'   \code{zones_to_key_matrix} and \code{key_matrix_to_zones}.
#'
#' @param zones A list where each entry is a zone. Each entry is a vector containing the indices of
#'   the spatial locations in that zone.
#'
#' @param key_matrix A matrix, where each row is a zone and each column is a location. The entry
#'   `[i, j]` is 1 if location `j` is a part of zone `i` and 0 otherwise.
#'
#' @return For `zones_to_key_matrix`, a matrix. To increase speed, the matrix is represented as a
#'   sparse matrix from the \code{\link[Matrix]{Matrix}} package. For `create_zones`, and
#'   `key_matrix_to_zones` a list of zones.
#'
#' @md
#' @seealso \code{\link{create_zones}},
#'   \code{\link[scanstatistics]{knn_zones}}, \code{\link[scanstatistics]{dist_to_knn}}
#' @examples
#' library("sf")
#' data("NM_county_sf")
#'
#' zones <- create_zones(NM_county_sf, max_k = 5)
#' key_matrix <- zones_to_key_matrix(zones)
#'
#' zones <- key_matrix_to_zones(key_matrix)
#' @name switch_zone_rep
NULL

#' @export
#' @rdname switch_zone_rep
zones_to_key_matrix <- function(zones) {
  n_zones <- length(zones)
  n_locations <- max(unlist(zones))
  key_matrix <- matrix(rep(0, n_zones * n_locations), ncol = n_zones)
  for (i in seq_along(zones)) {
    key_matrix[zones[[i]], i] <- 1
  }
  key_matrix <- t(key_matrix)
  return(Matrix::Matrix(key_matrix))
}

#' @export
#' @rdname switch_zone_rep
key_matrix_to_zones <- function(key_matrix) {
  # Builds a list of zones from a key_matrix
  zones <- apply(key_matrix, 1, function(x) {
    which(x == 1)
    })
  return(zones)
}

#' @title Turn a long data frame into a wide matrix
#'
#' @description A friendly wrapper for \code{\link[tidyr]{pivot_wider}} that takes a long data.frame
#'   and transforms it into a wide matrix. This is the format expected as input for the #'
#'   \code{\link[scanstatistics]{scanstatistics}} package.
#'
#' @inheritParams spacetime_data
#' @param value_col An expression defining the values in in `spacetime_data` to be pivoted into the
#'   wide matrix. This argument uses
#'   \href{https://dplyr.tidyverse.org/reference/dplyr_data_masking.html}{data masking} and should
#'   be specified as bare code (not a string). Can also be a scalar, in which case all entries in
#'   the wide matrix will be equal to `value_col`.
#'
#' @details Functions for calculating scan statistics in the
#'   \code{\link[scanstatistics]{scanstatistics}} package expect case counts and baseline estimates
#'   to be formatted as matrices, where the rows correspond to time (organized from earliest to
#'   latest) and the columns correspond to space.
#'
#' `pivot_for_scan` takes `spacetime_data`, and pivots the data into that format. The rows are
#' ordered by the `id_time` column from smaller to largest, and the columns are ordered by the
#' `id_space` column in `spacetime_data`, also from smallest to largest. `pivot_for_scan` expects
#' the data to be complete, so that every combination of `id_space` and `id_time` is associated with
#' exactly one value. If some combination of `id_space` and `id_time` has no corresponding value in
#' `spacetime_data`, then that entry in the returned matrix will be `<NA>`.
#'
#'
#' @return A matrix, with rows corresponding to values of `id_time` and columns corresponding to
#'   `id_space`. Exception: if `value_col = NULL`, then returns NULL.
#'
#' @export
#' @md
#' @examples
#' df <- data.frame(vals = c(1, 2, 3, 4, 5, 6),
#'                  id_space = rep(1:3, 2),
#'                  id_time = rep(1:2, each = 3))
#'
#' pivot_for_scan(df, vals)
#'
#'
#' # Non-exist combinations create NA's
#' incomplete_df <- df[1:5, ]
#' pivot_for_scan(incomplete_df, vals)
#'
#' # Returns NULL:
#' pivot_for_scan(df, NULL)
pivot_for_scan <- function(spacetime_data, value_col) {

  validate_spacetime_data(spacetime_data)

  n_distinct <- nrow(dplyr::distinct(spacetime_data, .data$id_space, .data$id_time))
  if (nrow(spacetime_data) != n_distinct) {
    rlang::abort(
      "All combinations of 'id_space' and 'id_time' must be associated with at most one row",
      class = "error_bad_spacetimedata")
  }

  x <- spacetime_data %>%
    dplyr::transmute(id_space = .data$id_space,
                     id_time = .data$id_time,
                     y = {{value_col}})
  if (ncol(x) == 2) { # Hack to deal with the case when value_col = NULL
    return(NULL)
  }

  x <- x %>%
    dplyr::arrange(.data$id_time, .data$id_space) %>%
    tidyr::pivot_wider(id_cols = "id_time",
                       names_from = "id_space",
                       values_from = "y") %>%
    dplyr::arrange(.data$id_time) %>%
    as.data.frame()
  rownames(x) <- x$id_time
  x$id_time <- NULL
  return(as.matrix(x))
}

unpivot_parallel_alarms <- function(parallel_alarm_res, values_to = ".action_level",
                                    row_names = NULL, col_names = NULL) {
  df <- as.data.frame(parallel_alarm_res)
  if (!is.null(row_names)) {
    rownames(df) <- row_names
  } else if (is.null(rownames(parallel_alarm_res))) {
    rownames(df) <- seq_len(nrow(df))
  }

  if (!is.null(col_names)) {
    colnames(df) <- col_names
  } else if (is.null(colnames(parallel_alarm_res))) {
    colnames(df) <- seq_len(ncol(df))
  }

  df %>%
    tibble::rownames_to_column("id_time") %>%
    tidyr::pivot_longer(cols = -"id_time", names_to = "id_space", values_to = values_to) %>%
    dplyr::mutate(id_space = as.numeric(.data$id_space),
                  id_time = as.numeric(.data$id_time)) %>%
    tibble::as_tibble() %>%
    validate_spacetime_data()
}

#' @title Identify the category of an alarm function
#'
#' @description `statsurv` uses two different forms of alarm functions: scan-type alarms, which
#'   calcualte an alarm statistic over a large number of spatial zones, and parallel-type alarms,
#'   which calculate an alarm statistic over individual spatial locations. These are identifiable by
#'   whether the name of the alarm function starts with "scan_" or "parallel_".
#'
#' @param alarm_function_name The name of the alarm function in question, as a character string.
#'
#' @return Either "scan" or "parallel".
#' @export
#' @keywords internal
#' @examples
#' calculate_alarm_type("scan_eb_poisson")
#' calculate_alarm_type("parallel_cusum_poisson")
calculate_alarm_type <- function(alarm_function_name) {
  prefix <- strsplit(alarm_function_name, "_")[[1]][[1]]
  return(prefix)
}
