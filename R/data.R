#' @title Geographical boundaries for counties in New Mexico
#'
#' @description A simple features collection containing the geographical boundaries for all counties
#'   in New Mexico, as of 2010. To avoid errors when looking at counties over time, Cibola county is
#'   not included.
#'
#' @format A Simple Features collection with 32 rows and 2 variables:
#' \describe{
#'   \item{id_space}{An integer label associated with each county (1-32)}
#'   \item{county}{The name of the county, in all lower case
#'   ("bernalillo" - "valencia")}
#'  }
#'
#' @note To save memory, county boundaries were simplified using the \code{\link[sf]{st_simplify}}
#'    command and therefore do not exactly match the source shapefiles.
#'
#' @source \url{http://rgis.unm.edu/rgis6/}
"NM_county_sf"


#' @title Population and brain cancer cases in New Mexico, 1973 - 1991
#'
#' @description A data set containing the population count an dnumber of brain cancer cases in each
#'   county in New Mexico for the years 1973 - 1991. The data has been formatted to be a valid
#'   \code{\link[=spacetime_data]{spacetime data frame}}
#'
#' @format A data frame with 608 rows and 7 columns:
#' \describe{
#'   \item{id_time}{An integer label associated with each year (1-19)}
#'   \item{id_space}{An integer label associated with each county (1-32)}
#'   \item{year}{The year the cases were recorded (1973-1991)}
#'   \item{county}{The name of the county, in all lower cases, with no spaces ("bernalillo" -
#'   "valencia")}
#'   \item{population}{The popoulation of the county in that year. Interpolated from censuses in
#'   1973, 1982, and 1991 (977-490248)}
#'   \item{count}{The number of brain cancer cases reported in that county and year (0-34)}
#'   \item{baseline_est}{A simple baseline estimate of the expected number of cases based on the
#'   population (0.044-22.5)}
#'  }
#'
#' @source \url{https://github.com/BenjaK/scanstatistics}
"NM_data"

#' @title Quarterly Approval Ratings of US Presidents
#'
#' @description A data frame containing the approximately quarterly approval rating for the
#'   President of the United States, from 1945 - 1974. Converted to a data frame from the
#'   \code{\link[datasets]{presidents}} time series.
#'
#' @format A data frame with 120 rows and 3 columns:
#' \describe{
#'   \item{approval_rating}{The quarterly approval rating of the US President, as a percent (23-87)}
#'   \item{date}{The year, as a decimal (1945.25 - 1975.00)}
#'   \item{election_year}{1 if the current year was presidential election year, 0 otherwise (0/1)}
#'}
#'
#' @source \code{\link[datasets]{presidents}}
"presidents_df"
