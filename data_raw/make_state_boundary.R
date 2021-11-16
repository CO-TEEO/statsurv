library("sf")
co_state_boundary <- st_read("data_raw/geospatial_data/colorado_state_boundary/Colorado_State_Boundary.shp",
                             stringsAsFactors = FALSE)
co_state_boundary <- co_state_boundary %>%
  dplyr::transmute(name = "colorado")
plot(co_state_boundary)
usethis::use_data(co_state_boundary, overwrite = TRUE)
