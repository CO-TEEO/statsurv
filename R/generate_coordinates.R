#' @title Generate a set of spatial coordinates
#'
#' @description Generate a set of spatial coordinates, given a collection of polygons and optionally
#'   a spatial boundary.
#'
#' @param surveillance_boundary A SpatialPolygon* or sf object that defines the boundary of the
#'   study area. If NULL, the entirity of \code{space_division} will be returned.
#' @param space_division What spatial divisions should be used. Allowed options are
#'   'denver_censustract_fips_2019', 'co_county_fips_2019', 'co_hsr_2019', 'co_state_boundary', or a
#'   spatial polygons data frame or sf object. The 4 non-filename options instruct the function to
#'   use sptial divisions that are included with the `statsurv`` package, and were processed on 19
#'   May 2020. They correspond to all of the census tracts around Denver and the counties, health
#'   statistics regions, and states within Colorado.
#' @param inside_fraction_cutoff What fraction of the unit must be inside the surveillance_boundary
#'   for the unit to be included? Default is 0 (any unit that touches the boundary will be
#'   included).
#'
#' @return A spatial polygons data frame that is a valid
#'   \code{\link[gridcoord:gc_gridcoord]{gridcoord}} object. A spdf object is returned, regardless
#'   of the format of `surveillance_boundary` or `space_division`.
#' @export
#' @md
#' @examples
#' surveillance_area <- statsurv::denver_metro_area
#'
#' # Returns all counties that have at least 10% of their area within
#' # the greater Denver Metro area
#' generate_study_area(surveillance_area,
#'                     "co_county_fips_2019",
#'                     inside_fraction_cutoff = 0.1)
#'
#' # Returns all of the Colorado Health Statistics Regions
#' generate_study_area(space_division = "co_hsr_2019")
st_select_overlap <- function(x, y, cutofff = 0) {
  stopifnot(is.numeric(cutoff),
            length(cutoff) == 1,
            cutoff >= 0,
            cutoff <= 1)

  intersection <- sf::st_intersection(sf::st_geometry(x), sf::st_geometry(y))
  wanted_rows <- attr(intersection, "idx")[, 1]
  if (cutoff > 0) {
    full_area <- sf::st_area(x[wanted_rows, ] )
    frac_area <- as.numeric(sf::st_area(intersection)/full_area)
    if (cutoff == 1) {
      frac_area <- round(frac_area, 2)
    }
    wanted_rows <- wanted_rows[frac_area > cutoff]
  }
  x[wanted_rows, ]
}
#
# generate_study_area <- function(surveillance_boundary = NULL,
#                                 space_division = "co_censustract_fips_2019",
#                                 inside_fraction_cutoff = 0) {
#   ### Argument checks and computations #########################################
#   check_type(inside_fraction_cutoff, "numeric")
#   check_scalar(inside_fraction_cutoff)
#   if (inside_fraction_cutoff < 0 || inside_fraction_cutoff > 1) {
#     stop("inside_fraction_cutoff must be between 0 and 1")
#   }
#   space_units <- parse_spacedivision_name(space_division)
#   if (is.null(surveillance_boundary)) {
#     return(gridcoord::gc_gridcoord(space_units))
#   } else {
#     if (is_type(space_units, "sf")) {
#       space_units <- methods::as(space_units, "Spatial")
#     }
#     check_type(surveillance_boundary, c("sf", "SpatialPolygons"))
#     if (is_type(surveillance_boundary, "sf")) {
#       surveillance_boundary <-  methods::as(surveillance_boundary,
#                                             "Spatial")
#     }
#   }
#
#   ### Computation if !is.null(surveillance_boundary) ###########################
#   surveillance_boundary <- sp::spTransform(surveillance_boundary,
#                                            sp::CRS(sp::proj4string(space_units)))
#   selected_units <- suppressWarnings(space_units[surveillance_boundary, ])
#
#   if (length(selected_units) == 0) {
#     stop("No spatial units selected. Check your boundary or decrease inside_fraction_cutoff")
#   }
#
#   if (inside_fraction_cutoff > 0) {
#     frac_area <- raster::area(
#       suppressWarnings(raster::intersect(selected_units, surveillance_boundary))) /
#       raster::area(selected_units)
#     # Round the answer to make inside_fraction_cutoff = 1 behave reasonably
#     frac_area <- round(frac_area, 2)
#     selected_units <- selected_units[frac_area >= inside_fraction_cutoff, ]
#   }
#   rownames(selected_units@data) <- NULL
#   return(selected_units)
# }
#
# parse_spacedivision_name <- function(space_division) {
#   if (is_type(space_division, "SpatialPolygons")) {
#     return(space_division)
#   }
#   if (is_type(space_division, "sf")) {
#     return(methods::as(space_division, "Spatial"))
#   }
#   if (is.character(space_division)) {
#     check_scalar(space_division)
#     # We have a number of preprocessed data files in the package that can be loaded up
#     # Weird to use my own package, but believe this is the approach recommended by
#     # ?data
#     space_units <- switch(space_division,
#                           "denver_censustract_fips_2019" =
#                             statsurv::denver_censustract_fips_2019,
#                           "co_county_fips_2019" =
#                             statsurv::co_county_fips_2019,
#                           "co_hsr_2019" =
#                             statsurv::co_hsr_2019,
#                           "co_state_boundary" =
#                             statsurv::co_state_boundary,
#                           NULL)
#     if (!is.null(space_units)) {
#       return(space_units)
#     }
#   }
#   stop("Unable to parse the given input for the space_division ",
#        "Allowed options are 'co_censustract_fips_2019', 'co_county_fips_2019', 'co_hsr_2019', ",
#        "'co_state_boundary', or a spatial polygons data frame.")
# }
