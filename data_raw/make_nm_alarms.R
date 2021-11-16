library("scanstatistics")
library("magrittr")
library("gridcoord")
library("statsurv")
library("sf")
data(NM_popcas)
NM_popcas$county <- as.character(NM_popcas$county)
NM_popcas$date_label <- as.character(NM_popcas$year)

nm_county_fips_2010 <- statsurv::nm_county_fips_2010
nm_county_fips_2010$county <- gsub(" ", "", tolower(nm_county_fips_2010$county_name))
nm_county_fips_2010 <- nm_county_fips_2010[nm_county_fips_2010$county != "cibola", ]
nm_county_fips_2010 <- gc_gridcoord(nm_county_fips_2010, "county")
nm_county_coord <- nm_county_fips_2010

year_coord <- generate_date_range(lubridate::ymd("1973-01-01"),
                                  lubridate::ymd("1991-01-01"),
                                  time_division = "year")

glm_func <- function(space_coord, time_coord, data_for_model) {
  mod <- glm(count ~ year,
             family = poisson(link = "log"),
             offset = log(population),
             data = data_for_model)
  return(list(fit = mod,
              data = data_for_model))
}

fits_and_data <- loop_model(nm_county_fips_2010, year_coord, NM_popcas,
                            outcome_col = "count",
                            path_to_model = glm_func)
all_fits <- fits_and_data[[1]]
all_data <- fits_and_data[[2]]

all_yhats <- loop_extract_yhat(nm_county_fips_2010, year_coord, all_fits, all_data,
                               yhat_extractor_name = "extract",
                               use_surveillance_residuals = FALSE)



all_alarms <- loop_alarm_function(nm_county_fips_2010, year_coord, all_yhats, all_data,
                                  outcome_col = "count",
                                  alarm_function_name = "scan_eb_poisson",
                                  max_k = 3,
                                  n_mcsim = 100)
nm_scan_alarms <- all_alarms
nm_cusum_alarms <- loop_alarm_function(nm_county_fips_2010, year_coord, all_yhats, all_data,
                                       outcome_col = "count",
                                       alarm_function_name = "parallel_cusum_poisson")

usethis::use_data(nm_scan_alarms, overwrite = TRUE)
usethis::use_data(nm_cusum_alarms, overwrite = TRUE)
usethis::use_data(nm_county_coord, overwrite = TRUE)
