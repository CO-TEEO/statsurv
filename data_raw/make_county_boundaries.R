library("sf")
geospatial_folder <- file.path("data_raw",
                               "geospatial_data")

county_filename <- file.path(geospatial_folder,
                             "colorado_county_boundaries",
                             "Colorado_County_Boundaries.shp")

county_spdf <- rgdal::readOGR(county_filename,
                              verbose = FALSE,
                              stringsAsFactors = FALSE)

county_sf <- st_as_sf(county_spdf)
old_crs <- st_crs(county_sf)
new_crs <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lon_0=-96.0 +datun=NAD83 +ellps=GRS80 +lat_0=23.0")
county_proj <- st_transform(county_sf, new_crs)

county_proj_coarse <- st_simplify(county_proj, dTolerance = 100)
county_latlon_coarse <- st_transform(county_proj_coarse, old_crs)


# co_county_fips_2019 <- as(county_latlon_coarse, "Spatial")
co_county_fips_2019 <- county_latlon_coarse %>%
  dplyr::transmute(fips_str = US_FIPS)
usethis::use_data(co_county_fips_2019, overwrite = TRUE)

