library("sf")
geospatial_folder <- file.path("data_raw",
                               "geospatial_data")

hsr_filename <- file.path(geospatial_folder,
                             "colorado_health_statistics_regions",
                             "CDPHE_Colorado_Health_Statistics_Regions.shp")

hsr_spdf <- rgdal::readOGR(hsr_filename,
                              verbose = FALSE,
                              stringsAsFactors = FALSE)

hsr_sf <- st_as_sf(hsr_spdf)
old_crs <- st_crs(hsr_sf)
new_crs <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lon_0=-96.0 +datun=NAD83 +ellps=GRS80 +lat_0=23.0")
hsr_proj <- st_transform(hsr_sf, new_crs)

hsr_proj_coarse <- st_simplify(hsr_proj, dTolerance = 100)
hsr_latlon_coarse <- st_transform(hsr_proj_coarse, old_crs) %>%
  dplyr::arrange(as.numeric(REGION))

# co_hsr_2019 <- as(hsr_latlon_coarse, "Spatial")
co_hsr_2019 <- hsr_latlon_coarse %>%
  dplyr::transmute(hsr = sprintf("hsr_%02d", as.numeric(REGION)))
usethis::use_data(co_hsr_2019, overwrite = TRUE)
