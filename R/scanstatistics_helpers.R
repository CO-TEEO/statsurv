#' @title Change how spatial zones are represented
#'
#' @description Scan-type alarm functions work by computing an alarm function over a very large
#'   number of spatial *zones*, where each zone consists of one of more neighboring spatial
#'   locations. Information about which spatial locations belong to which zone can be specified in
#'   the form of a list, where each entry in the list contains the indexes of the locations in a
#'   particular zone, or in the form of a matrix, where each row represents a zone and each column
#'   represents a location. A set of zones can be generated using the
#'   \code{\link{space_coord_to_zones}} function, and conversion between the list- and
#'   matrix-representation of the zones is accomplished through the functions
#'   \code{build_key_matrix} and \code{build_zones}.
#'
#' @param zones A list where each entry is a zone. Each entry is a vector containing the indices of
#'   the spatial locations in that zone.
#'
#' @param key_matrix A matrix, where each row is a zone and each column is a location. The entry
#'   `[i, j]` is 1 if location `j` is a part of zone `i` and 0 otherwise.
#'
#' @return For `build_key_matrix`, a matrix. To increase speed, the matrix is represented as a
#'   sparse matrix from the \code{\link[Matrix]{Matrix}} package. For `build_zones`, a list of
#'   zones.
#'
#' @md
#' @seealso \code{\link{space_coord_to_zones}},
#'   \code{\link[scanstatistics]{knn_zones}}, \code{\link[scanstatistics]{dist_to_knn}}
#' @examples
#' space_coord <- co_county_fips_2019
#' zones <- space_coord_to_zones(space_coord, max_k = 5)
#' key_matrix <- build_key_matrix(zones)
#'
#' zones <- build_zones(key_matrix)
#' @name switch_zone_rep
NULL

#' @export
#' @rdname switch_zone_rep
build_key_matrix <- function(zones) {
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
build_zones <- function(key_matrix) {
  # Builds a list of zones from a key_matrix
  zones <- apply(key_matrix, 1, function(x) {which(x == 1)})
  return(zones)
}

#' @title Create zones from spatial coordinates
#'
#' @description Scan-type alarm functions work by computing an alarm function over a very large
#'   number of spatial *zones*, where each zone consists of one of more neighboring spatial
#'   locations. Zones can be many sizes, from a single location up to an aggregation of `max_k`
#'   locations, and each location can belong to multiple zones. `space_coord_to_zones` aggregates
#'   individual spatial locations into a set of zones using a k-nearest neighbors approach. For each
#'   location, there will be a zone containing the location itself, a zone containing the location
#'   and its nearest neighbor, a zone containing the location and its two nearest-neighbors, up to
#'   the maximum size.
#'
#' @param space_coord Either a SpatialPolygonsDataFrame or a simple-features (sf) collection
#'   describing the spatial locations to combine into zones.
#' @param max_k The maximum number of locations to be included in any one zone.
#'
#' @return A list of integer vectors, specifying which locations belong to which zones. The integers
#'   identify locations based on which row in `space_coord` they are associated with.
#' @export
#' @md
#' @importFrom magrittr %>%
#' @examples
#' library("sf")
#' space_coord_to_zones(co_county_fips_2019, 5)
#' # 297 zones created from 64 locations
space_coord_to_zones <- function(space_coord, max_k) {
  check_type(space_coord, c("Spatial", "sf"))
  check_scalar_type(max_k, "integer")
  if (max_k > nrow(space_coord) || max_k < 1) {
    stop("max_k must be an integer between 1 and the number of spatial locations")
  }

  if (is_type(space_coord, "sf")) {
    space_coord <- methods::as(space_coord, "Spatial")
  }
  zones <- space_coord %>%
    sp::spDists(x = ., y = ., longlat = TRUE) %>%
    scanstatistics::dist_to_knn(k = max_k) %>%
    scanstatistics::knn_zones()
  return(zones)
}

#' @title Turn a long data.frame into a wide matrix
#'
#' @description A friendly wrapper for \code{\link[tidyr]{pivot_wider}} that takes a long
#'   data.frame and transforms it into a wide matrix. This is the format for data used by the
#'   \code{\link[scanstatistics]{scanstatistics}} package and other scan-type alarm functions.
#'
#' @param df A long (a.k.a. tidy) data.frame.
#' @param value_col The name of the column that contains the data to be pivoted into the wide
#'   matrix. All columns other than `value_col` in `df` will be discarded.
#' @param column_coord,row_coord The columns in `df` that will become the column or row names of the
#'   returned matrix. These can each be either a \itemize{
#'   \item A character string, specifying the name of the column in `df`. In this case, the column
#'   in `df` must be sortable, and columns or rows in the returned matrix will be sorted from first
#'   to last.
#'   \item A \code{\link[gridcoord:gc_gridcoord]{gridcoord}} object corresponding to a column in
#'   `df`. In this case, the rows or columns in the returned matrix will be arranged to match the
#'   order of the labels in the gridcoord object.
#'   }
#'
#' @details `df` must contain at least 3 columns, containing the values, column names, and rownames
#'   respectively. Many scan-type alarm functions take input data in the form of a matrix where the
#'   rows correspond to time (organized from earliest to latest) and the columns correspond to
#'   space.
#'
#' `pivot_for_scan` is best used for complete datasets, where every combination of `row_coord` and
#' `column_coord` is associated with exactly one value. If there are multiple values associated with
#' a particular combination of `row_coord` and `column_coord`, `pivot_for_scan` will give a warning
#' and return a matrix where some of the entires are lists. If there a no values
#' associated with a combination of `row_coord` and `column_coord` with no corresponding value in
#' `df`, then that entry in the returned matrix will be `<NA>`. However, if the returned matrix
#' would have entire rows or columns filled with only `<NA>`, these are silently removed.
#'
#' @return A matrix, with rows corresponding to entries in `row_coord` and columns corresponding to
#'   `column_coord`.
#'
#' @export
#' @md
#' @examples
#' df <- data.frame(vals = c(1, 2, 3, 4, 5, 6),
#'                  space = c("s1", "s2", "s3", "s1", "s2", "s3"),
#'                  time = c("t1", "t1", "t1", "t2", "t2", "t2"),
#'                  stringsAsFactors = FALSE)
#'
#' pivot_for_scan(df, "vals", "space", "time")
#'
#' # Non-exist combinations create NA's
#' incomplete_df <- df[1:5, ]
#' pivot_for_scan(incomplete_df, "vals", "space", "time")
#'
#' # gridcoords can rearrange the rows or columns of the matrix
#' space_coord <- data.frame(space = c("s2", "s3", "s1"),
#'                           stringsAsFactors = FALSE)
#' pivot_for_scan(df, "vals", space_coord, "time")
#'
#' # Extra rows/columns are silently dropped
#' time_coord <- data.frame(time = c("t1", "t2", "t3"),
#'                          stringsAsFactors = FALSE)
#' pivot_for_scan(df, "vals", "space", time_coord)

pivot_for_scan <- function(df, value_col, column_coord, row_coord) {
  check_type(df, "data.frame")
  check_scalar_type(value_col, "character")


  if (is.character(column_coord)) {
    colnames_col <- column_coord
    column_ordering <- sort(unique(df[[colnames_col]]))
  } else {
    colnames_col <- gridcoord::gc_get_name(column_coord)
    column_ordering <- gridcoord::gc_get_labels(column_coord)
    column_ordering <- column_ordering[column_ordering %in% df[[colnames_col]]]
  }
  if (is.numeric(column_ordering)) {
    column_ordering <- as.character(column_ordering)
  }

  if (is.character(row_coord)) {
    rownames_col <- row_coord
    row_ordering <- sort(unique(df[[rownames_col]]))
  } else {
    rownames_col <- gridcoord::gc_get_name(row_coord)
    row_ordering <- gridcoord::gc_get_labels(row_coord)
  }


  x <- df %>%
    dplyr::select(colnames_col, value_col, rownames_col) %>%
    tidyr::pivot_wider(names_from = colnames_col, values_from = value_col) %>%
    dplyr::arrange(match(.data[[rownames_col]], row_ordering)) %>%
    as.data.frame()
  rownames(x) <- x[, rownames_col, drop = TRUE]
  x[, rownames_col] <- NULL
  x <- x %>%
    dplyr::select(column_ordering)
  if (!identical(colnames(x), column_ordering)) {
    stop("Error in assigning column names - the order in the wide data frame is somehow incorrect")
  }
  return(as.matrix(x))
}



#' @title Ensure that zone information is repesented as a list
#'
#' @description Information about which spatial locations belong to which zone can be specified in
#'   the form of a list, where each entry in the list contains the indexes of the locations in a
#'   particular zone, or in the form of a matrix, where each row represents a zone and each column
#'   represents a location. `ensure_zones` accepts either representation as input, and always
#'   returns zones represented as a list.
#'
#' @param zone_info Either:
#'   \itemize{
#'     \item A list where each entry is a zone. Each entry is a vector containing the indices of
#'   the spatial locations in that zone.
#'     \item A matrix, where each row is a zone and each column is a location. The entry `[i, j]`
#'     is 1 if location `j` is a part of zone `i` and 0 otherwise.
#'   }
#'
#' @return A list, where each entry is a zone.
#' @export
#' @md
#' @examples
#' library("sf")
#' zone_list <- space_coord_to_zones(co_county_fips_2019, 5)
#' zone_matrix <- build_key_matrix(zone_list)
#'
#' \dontrun{
#' # Both give the same results
#' ensure_zones(zone_list)
#' ensure_zones(zone_matrix)
#' }
ensure_zones <- function(zone_info) {
  if (is_type(zone_info, c("matrix", "Matrix"))) {
    zones <- build_zones(zone_info)
  } else {
    zones <- zone_info
  }
  return(zones)
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