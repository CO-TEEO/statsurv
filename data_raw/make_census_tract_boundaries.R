library("sf")
geospatial_folder <- file.path("data_raw",
                               "geospatial_data")

ct_filename <- file.path(geospatial_folder,
                             "colorado_census_tract_boundaries",
                             "Colorado_Census_Tract_Boundaries.shp")

ct_spdf <- rgdal::readOGR(ct_filename,
                          verbose = FALSE,
                          stringsAsFactors = FALSE)


dmb <- st_read("data_raw/geospatial_data/denver_metro_boundary/denver_metro_boundary.shp")
dmb <- st_transform(dmb, st_crs(ct_spdf))
dmb$Name <- "Denver Metro Area"
denver_metro_area <- dmb
usethis::use_data(denver_metro_area, overwrite = TRUE)


denver_censustract_fips_2019 <- generate_study_area(dmb, ct_spdf[, "FIPS"])
denver_censustract_fips_2019$OBJECTID <- NULL
denver_censustract_fips_2019$fips_str <- denver_censustract_fips_2019$FIPS
denver_censustract_fips_2019$FIPS <- NULL

denver_censustract_fips_2019 <- st_as_sf(denver_censustract_fips_2019)

usethis::use_data(denver_censustract_fips_2019, overwrite = TRUE)
