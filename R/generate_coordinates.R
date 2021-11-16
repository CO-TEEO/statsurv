#' @title Generate a gridcoord covering a specified date range
#'
#' @description \code{generate_date_range} generates set a span coordinates following the
#'   conventions of the gridcoord package (\code{\link[gridcoord]{gc_gridcoord}}) where each span is
#'   the same length.
#'
#' @param start_date The first day that must be included in the date range
#' @param end_date The last day that must be included in the date range
#' @param time_division The length of each interval
#' @param week_start What day do weeks start on? Can only be 7 (Sunday) or 1 (Monday). Only used if
#'   \code{time_division = "week"}
#' @param coordinate_name The name to use for the first column of the generated data.frame,
#'   containing the labels of the time spans.
#'
#' @details The time spans returned by \code{generate_date_range} are always aligned to standard
#'   breaks in time. If \code{time_division} is "month", then each time span will start on the 1st
#'   of the month and end on the last of the month, even if \code{start_date} is (for example) the
#'   14th. The time spans are inclusive, meaning that they will always add dates before
#'   \code{start_date} or after \code{end_date} to align the time spans with breaks in the calendar.
#'
#' Assigning days of the year to a specific week is remarkably confusing, as weeks can span multiple
#' years. The convention used here is to assign weeks to the year in which they have the majority of
#' their days. While this assigns each day to a single week, it can lead to some oddities around the
#' new year. If we assign weeks starting on Sunday, then 29 Dec 2019 is the first day of Week 1 in
#' 2020, while 02 Jan 2016 is the last day of week 52 in 2015. Some years have a different number of
#' weeks than other years. The weeks assigned by \code{generate_date_range} are designed to match
#' those used by the CDC in their reporting.
#'
#' @return A three column gridcoord dataframe, where the first column is the name of the time span,
#'   the second column is the starting date, and the third column is the ending date. Each row
#'   represents a time span. The first row will always include the \code{start_date}, and the last
#'   row will always include the \code{end_date}. The returned time spans are left-closed,
#'   right-open. The column names are `coordinate_name`, "start_date", and "end_date".
#'
#' @references \url{https://wwwn.cdc.gov/nndss/document/MMWR_Week_overview.pdf}
#' @export
#'
#' @examples
#' library("lubridate")
#'
#' generate_date_range(ymd("2012-01-01"), ymd("2012-12-15"), "month")
#' generate_date_range(ymd("2012-04-04"), ymd("2050-12-15"), "year")
#' generate_date_range(ymd("2012-04-04"), ymd("2050-12-15"), "week")
#' generate_date_range(ymd("2012-04-04"),
#'                     ymd("2050-12-15"),
#'                     "week",
#'                     week_start = 1)
generate_date_range <- function(start_date,
                                end_date,
                                time_division = c("month", "quarter", "year", "week", "day"),
                                coordinate_name = "date_label",
                                week_start = 7) {

  ### Argument checks ##########################################################
  time_division <- match.arg(time_division)
  check_type(start_date, "Date", throw_error = TRUE)
  check_type(end_date, "Date", throw_error = TRUE)
  if (start_date > end_date) {
    stop("start_date must be earlier than end_date")
  }
  check_scalar(start_date, throw_error = TRUE)
  check_scalar(end_date, throw_error = TRUE)
  check_scalar(time_division, throw_error = TRUE)
  check_scalar(week_start, throw_error = TRUE)
  if (!week_start %in% c(1, 7)) {
    stop("week_start must be either 1 or 7")
  }
  check_scalar_type(coordinate_name, "character")

  ### Actual computation #######################################################
  # First - calculate the start date...
  # For weeks, it's important to
  first_start_date <- lubridate::floor_date(start_date, time_division, week_start)
  last_start_date <- lubridate::floor_date(end_date, time_division, week_start)
  start_dates <- seq(first_start_date, last_start_date, by = time_division)

  first_end_date <- lubridate::ceiling_date(start_date, time_division, week_start = week_start)
  last_end_date <- lubridate::ceiling_date(end_date, time_division, week_start = week_start)
  end_dates <- seq(first_end_date, last_end_date, by = time_division)

  # What do I want the labels to be?
  if (time_division == "month") {
    labels <- format(start_dates, "%Y-%m")
  } else if (time_division == "quarter") {
    labels <- paste0(lubridate::year(start_dates),
                     "Q",
                     lubridate::quarter(start_dates))
  } else if (time_division == "year") {
    labels <- format(start_dates, "%Y")
  } else if (time_division == "week") {
    if (week_start == 7) {
      wks <- lubridate::epiweek(start_dates)
    } else {
      wks <- lubridate::isoweek(start_dates)
    }
    yrs <- lubridate::year(start_dates)
    mnths <- lubridate::month(start_dates)
    m <- mnths == 12 & wks == 1
    yrs[m] <- yrs[m] + 1
    labels <- paste0(yrs,
                     "-W",
                     sprintf("%02d", wks))
  } else if (time_division == "day") {
    labels <- format(start_dates, "%Y-%m-%d")
  }
  date_range <- data.frame(date_label = labels,
                           start_date = start_dates,
                           end_date = end_dates,
                           stringsAsFactors = FALSE)
  date_range[[coordinate_name]] <- date_range[["date_label"]]
  date_range <- date_range[, c(coordinate_name, "start_date", "end_date"), drop = FALSE]
  return(date_range)
}


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
generate_study_area <- function(surveillance_boundary = NULL,
                                space_division = "co_censustract_fips_2019",
                                inside_fraction_cutoff = 0) {
  ### Argument checks and computations #########################################
  check_type(inside_fraction_cutoff, "numeric")
  check_scalar(inside_fraction_cutoff)
  if (inside_fraction_cutoff < 0 || inside_fraction_cutoff > 1) {
    stop("inside_fraction_cutoff must be between 0 and 1")
  }
  space_units <- parse_spacedivision_name(space_division)
  if (is.null(surveillance_boundary)) {
    return(gridcoord::gc_gridcoord(space_units))
  } else {
    if (is_type(space_units, "sf")) {
      space_units <- methods::as(space_units, "Spatial")
    }
    check_type(surveillance_boundary, c("sf", "SpatialPolygons"))
    if (is_type(surveillance_boundary, "sf")) {
      surveillance_boundary <-  methods::as(surveillance_boundary,
                                            "Spatial")
    }
  }

  ### Computation if !is.null(surveillance_boundary) ###########################
  surveillance_boundary <- sp::spTransform(surveillance_boundary,
                                           sp::CRS(sp::proj4string(space_units)))
  selected_units <- suppressWarnings(space_units[surveillance_boundary, ])

  if (length(selected_units) == 0) {
    stop("No spatial units selected. Check your boundary or decrease inside_fraction_cutoff")
  }

  if (inside_fraction_cutoff > 0) {
    frac_area <- raster::area(
      suppressWarnings(raster::intersect(selected_units, surveillance_boundary))) /
      raster::area(selected_units)
    # Round the answer to make inside_fraction_cutoff = 1 behave reasonably
    frac_area <- round(frac_area, 2)
    selected_units <- selected_units[frac_area >= inside_fraction_cutoff, ]
  }
  rownames(selected_units@data) <- NULL
  return(selected_units)
}

parse_spacedivision_name <- function(space_division) {
  if (is_type(space_division, "SpatialPolygons")) {
    return(space_division)
  }
  if (is_type(space_division, "sf")) {
    return(methods::as(space_division, "Spatial"))
  }
  if (is.character(space_division)) {
    check_scalar(space_division)
    # We have a number of preprocessed data files in the package that can be loaded up
    # Weird to use my own package, but believe this is the approach recommended by
    # ?data
    space_units <- switch(space_division,
                          "denver_censustract_fips_2019" =
                            statsurv::denver_censustract_fips_2019,
                          "co_county_fips_2019" =
                            statsurv::co_county_fips_2019,
                          "co_hsr_2019" =
                            statsurv::co_hsr_2019,
                          "co_state_boundary" =
                            statsurv::co_state_boundary,
                          NULL)
    if (!is.null(space_units)) {
      return(space_units)
    }
  }
  stop("Unable to parse the given input for the space_division ",
       "Allowed options are 'co_censustract_fips_2019', 'co_county_fips_2019', 'co_hsr_2019', ",
       "'co_state_boundary', or a spatial polygons data frame.")
}
