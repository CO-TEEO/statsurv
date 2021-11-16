#' @title Geographical boundaries for counties in Colorado
#'
#' @description A SpatialPolygonsDataFrame containing the geographical boundaries for all counties
#'   in Colorado, as of 2019.
#'
#' @format A SpatialPolygonsDataFrame with 64 rows and 1 variables:
#' \describe{
#'   \item{fips_str}{The 5-digit code used to identify the county. A full list of IDs is available
#'   from the
#'   \href{https://www.census.gov/geographies/reference-files/2018/demo/popest/2018-fips.html}{US
#'   Census Bureau} ("08001" - "08125")}
#'  }
#'
#' @note To save memory, county boundaries were simplified using the \code{\link[sf]{st_simplify}}
#'    command and therfore do not exactly match the source shapefiles.
#'
#' @source \url{https://data-cdphe.opendata.arcgis.com/datasets/colorado-county-boundaries}
"co_county_fips_2019"

#' @title Geographical boundaries for counties in New Mexico
#'
#' @description A simple features collection containing the geographical boundaries for all counties
#'   in New Mexico, as of 2010
#'
#' @format A Simple Features collection with 33 rows and 2 variables:
#' \describe{
#'   \item{fips_str}{The 5-digit code used to identify the county. A full list of IDs is available
#'   from the
#'   \href{https://www.census.gov/geographies/reference-files/2018/demo/popest/2018-fips.html}{US
#'   Census Bureau} ("35001" - "35061")}
#'   \item{county_name}{The name of the county, with the first letter capitalized
#'   ("Bernalillo" - "Valencia")}
#'  }
#'
#' @note To save memory, county boundaries were simplified using the \code{\link[sf]{st_simplify}}
#'    command and therfore do not exactly match the source shapefiles.
#'
#' @source \url{http://rgis.unm.edu/rgis6/}
"nm_county_fips_2010"


#' @title Geographic boundaries for Colorado health statistics regions
#'
#' @description A SpatialPolygonsDataFrame containing the geographic boundaries for all health
#'   statistics regions in Colorado, as of 2019. The health statistics regions are aggregations of
#'   demographically similar counties and divide the state into 21 non-overlapping regions.
#'
#' @format A SpatialPolygonsDataFrame with 21 rows and 1 variables:
#' \describe{
#'   \item{hsr}{A string labeling the region, of the form "hsr_XX" ("hsr_01" - "hsr_21")}
#' }
#'
#' @note To save memory, region boundaries were simplified using the \code{\link[sf]{st_simplify}}
#'    command and therfore do not exactly match the source shapefiles.
#'
#' @source
#'   \url{https://data-cdphe.opendata.arcgis.com/datasets/cdphe-colorado-health-statistics-regions}
"co_hsr_2019"


#' @title Geographic boundary of the state of Colorado
#'
#' @description A SpatialPolygonsDataFrame containing the geographic boundaries of the state of
#'   Colorado.
#'
#' @format A SpatialPolygonsDataFrame with 1 row and 1 variables:
#' \describe{
#'   \item{name}{The name of the state ("colorado")}
#' }
#'
#'
#' @source \url{https://data-cdphe.opendata.arcgis.com/datasets/colorado-state-boundary}
"co_state_boundary"

#' @title Geographical boundaries for census tracts in the Denver Metro Area
#'
#' @description A SpatialPolygonsDataFrame containing the geographical boundaries for all census
#'   tracts in the Denver Metro Area. The approximate geographic boundary for metro area was defined
#'   as the beltway formed by highways CO-93, CO-470, E-470, the Northwest Parkway, US-36, and
#'   CO-170. Any census tract with any portion inside this ring is included in this dataset.
#'
#' @format A SpatialPolygonsDataFrame with 534 rows and 1 variables:
#' \describe{
#'   \item{fips_str}{The 11-digit code used to identify the county ("08001" - "08125")}
#'  }
#'
#'
#' @source
#'   \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
"denver_censustract_fips_2019"


#' @title Approximate boundary of the Denver metro area
#'
#' @description A SpatialPolygonsDataFrame describing the approximate geographic boundary for metro
#'   area was defined as the beltway formed by highways CO-93, CO-470, E-470, the Northwest Parkway,
#'   US-36, and CO-170.
#'
#' @format A SpatialPolygonsDataFrame with 1 rows and 1 variables:
#' \describe{
#'   \item{Name}{NA}
#'  }
#'
"denver_metro_area"


#' @title Spatial coordinate corresponding to the dataset NM_popcas
#'
#' @description A simple features collection containing the geographical boundaries for all counties
#'   in New Mexico that are included in the dataset \code{\link[scanstatistics]{NM_popcas}}
#'
#' @format A Simple Features collection with 32 rows and 2 variables:
#' \describe{
#'   \item{county}{The name of the county in all lower case and spaces removed
#'   ("bernalillo" - "valencia")}
#'  }
#'
#' @note To save memory, county boundaries were simplified using the \code{\link[sf]{st_simplify}}
#'    command and therfore do not exactly match the source shapefiles.
#'
#' @source \url{http://rgis.unm.edu/rgis6/}
"nm_county_coord"

#' @title Scanstatistics results for brain cancer in New Mexico, 1980 - 1991
#'
#' @description A list containing the results of repeated applying the Poisson scan statistic to
#'   cases of brain cancer in New Mexico at the county level. Baseline estimates were generated
#'   using a Poisson model, where the number of cases is a function of year and the population of
#'   the county was used at the offset. The maximum zone size is 3 counties.
#'
#' @format A list where each entry is either `NA` or the output of
#'   \code{\link[scanstatistics]{scan_eb_poisson}}, applied to cases of brain cancer in New Mexico
#'   at the county level.
#'
#' @md
"nm_scan_alarms"


#' @title CUSUM results for brain cancer in New Mexico, 1980 - 1991
#'
#' @description A list containing the results of repeated applying the CUSUM alarm statistic to
#'   cases of brain cancer in New Mexico at the county level. Baseline estimates were generated
#'   using a Poisson model, where the number of cases is a function of year and the population of
#'   the county was used at the offset.
#'
#' @format A list where each entry is either `NA` or the output of
#'   \code{\link{parallel_cusum_poisson}}, applied to cases of brain cancer in New Mexico
#'   at the county level.
#'
#' @md
"nm_cusum_alarms"
