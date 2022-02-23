library(scanstatistics)
library(tidyverse)
library(sf)
data(NM_popcas)

NM_data <- NM_popcas %>%
  dplyr::mutate(id_time = year - min(year) + 1,
                id_space = as.numeric(factor(county)),
                .before = 1) %>%
  dplyr::mutate(baseline_est = population * mean(count) / mean(population))

usethis::use_data(NM_data, overwrite = TRUE)

county_sf <- st_read("data_raw/education_byco2015/education_byco2015.shp") %>%
  st_transform("+proj=utm +zone=13") %>%
  dplyr::transmute(county = str_remove_all(tolower(NAME10), " "))

NM_county_sf <- full_join(NM_data, county_sf) %>%
  select(id_space, county, geometry) %>%
  distinct_all() %>%
  filter(!is.na(id_space)) %>%
  st_sf() %>%
  st_make_valid()

NM_county_sf <- NM_county_sf %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 10) %>%
  st_make_valid()

usethis::use_data(NM_county_sf, overwrite = TRUE)
