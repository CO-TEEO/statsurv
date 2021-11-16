census_tracts <- rgdal::readOGR(file.path("data_raw",
                                        "geospatial_data",
                                        "colorado_census_tract_boundaries",
                                        "Colorado_Census_Tract_Boundaries.shp"),
                              stringsAsFactors = FALSE,
                              verbose = FALSE)
census_tracts$OBJECTID <- NULL
census_tracts$fips_str <- census_tracts$FIPS
census_tracts$FIPS <- NULL
usethis::use_data(census_tracts, overwrite = TRUE)
