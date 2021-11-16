library("raster")
library("sf")
data_nm <- st_read("data_raw/education_byco2015/education_byco2015.shp")

county_data <- data_nm %>%
  dplyr::select(NAME10, GEOID10) %>%
  dplyr::rename(county_name = NAME10,
                fips_str = GEOID10)



old_crs <- st_crs(county_data)
new_crs <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lon_0=-96.0 +datun=NAD83 +ellps=GRS80 +lat_0=23.0")
county_proj <- st_transform(county_data, new_crs)

county_proj_coarse <- st_simplify(county_proj, dTolerance = 100)
county_latlon_coarse <- st_transform(county_proj_coarse, old_crs)


nm_county_fips_2010 <- county_latlon_coarse


usethis::use_data(nm_county_fips_2010, overwrite = TRUE)
