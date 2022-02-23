#' @title Create zones from spatial coordinates
#'
#' @description Scan-type alarm functions work by computing an alarm function over a very large
#'   number of spatial *zones*, where each zone consists of one of more neighboring spatial
#'   locations. Zones can be many sizes, from a single location up to an aggregation of `max_k`
#'   locations, and each location can belong to multiple zones. `create_zones` aggregates
#'   individual spatial locations into a set of zones using a k-nearest neighbors approach. For each
#'   location, there will be a zone containing the location itself, a zone containing the location
#'   and its nearest neighbor, a zone containing the location and its two nearest-neighbors, up to
#'   the maximum size.
#'
#' @param sf A simple-features (sf) collection describing the spatial locations to combine into
#'   zones. Each row of `sf` should represent a unique spatial location.
#' @param max_k The maximum number of locations to be included in any one zone.
#' @param min_k Optional. The minimum number of locations to be included in any one zone.
#'
#' @return A list of integer vectors, specifying which locations belong to which zones. The integers
#'   identify locations based on which row in `sf` they are associated with.
#' @export
#' @md
#' @importFrom magrittr %>%
#' @examples
#' library("sf")
#' data("NM_county_sf")
#' create_zones(NM_county_sf, max_k = 5, min_k = 2)
#' # 112 zones created from 32 locations
create_zones <- function(sf, max_k, min_k = 1) {
  stopifnot(inherits(sf, "sf"),
            rlang::is_scalar_integerish(max_k),
            rlang::is_scalar_integerish(min_k))

  if (max_k > nrow(sf) || max_k < 1) {
    stop("max_k must be an integer between 1 and the number of spatial locations")
  }
  if (min_k < 1 || min_k > max_k) {
    stop("min_k must be an integer between 1 and `max_k`")
  }


  zones <- sf %>%
    sf::st_geometry() %>%
    sf::st_centroid() %>%
    sf::st_distance(., ., by_element = FALSE) %>%
    scanstatistics::dist_to_knn(k = max_k) %>%
    scanstatistics::knn_zones()
  l <- purrr::map_dbl(zones, length)
  return(zones[l >= min_k])
}
