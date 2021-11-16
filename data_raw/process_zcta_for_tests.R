library("rgdal")
library("raster")
library("dplyr")
zcta_raw <- readOGR(file.path("data_raw",
                          "geospatial_data",
                          "Colorado_ZIP_Code_Tabulation_Areas_ZCTA",
                          "Colorado_ZIP_Code_Tabulation_Areas_ZCTA.shp"),
                stringsAsFactors = FALSE)
wanted_zips <- c("80203", #fully w/i denver metro
                 "80401", # partially w/i
                 "80746") # Not at all w/i
m <- zcta_raw$ZCTA5CE10 %in% wanted_zips
zcta <- zcta_raw[m, ]
zcta@data <- zcta@data %>%
  transmute(zcta_str = ZCTA5CE10)
writeOGR(obj = zcta, dsn = "tests/testthat/three_zips", layer = "three_zips", driver = "ESRI Shapefile")

