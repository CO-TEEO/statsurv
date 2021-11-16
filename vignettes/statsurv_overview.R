## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(statsurv)

## ----data-loading-------------------------------------------------------------
library("scanstatistics")
library("lubridate")
library("gridcoord")
library("dplyr")
library("sf")

data(NM_popcas)
NM_popcas <- NM_popcas %>%
  mutate(county = as.character(county))
head(NM_popcas)

nm_county_fips_2010 <- statsurv::nm_county_fips_2010 %>%
  mutate(county_name = gsub(" ", "", tolower(county_name))) %>% #Remove spaces, make lower case 
  rename(county = county_name) %>%
  dplyr::filter(county != "cibola") %>%
  gc_gridcoord("county")



yr_start <- min(NM_popcas$year)
yr_fin <- max(NM_popcas$year)
date_start <- ymd(paste0(yr_start, "-01-01"))
date_fin <- ymd(paste0(yr_fin, "-12-31"))
year_coord <- generate_date_range(date_start, date_fin, time_division = "year") %>%
  rename(year = date_label) %>%
  mutate(year = as.numeric(year))

## ----model-1------------------------------------------------------------------
mod <- glm(count ~ year,
           family = poisson(link = "log"),
           offset = log(population),
           data = NM_popcas)

## ----extract-yhat-------------------------------------------------------------
expected_cases <- extract_yhat(nm_county_fips_2010, year_coord, mod, NM_popcas)
plot(factor(expected_cases$county), expected_cases$yhat)

## -----------------------------------------------------------------------------
expected_case_samples <- sample_yhat(nm_county_fips_2010, year_coord, mod, NM_popcas, 
                                     n_samples = 20)
head(as_tibble(expected_case_samples))

## ----scan-eb-pois-------------------------------------------------------------
wide_cases <- pivot_for_scan(NM_popcas, "count", nm_county_fips_2010, year_coord)
wide_baseline <- pivot_for_scan(expected_cases, "yhat", nm_county_fips_2010, year_coord)
zones <- space_coord_to_zones(nm_county_fips_2010, max_k = 10)

scan_res <- scan_eb_poisson(wide_cases, zones, baselines = wide_baseline, n_mcsim = 100)
print(scan_res)

extract_clusters(scan_res, zones, "score", allow_overlap = FALSE, max_allowed = 5)

## ----glm-func-----------------------------------------------------------------
glm_func <- function(space_coord, time_coord, data_for_model) {
  mod <- glm(count ~ year,
             family = poisson(link = "log"),
             offset = log(population),
             data = data_for_model)
  to_return <- list(fit = mod,
                    data = data_for_model)
  return(to_return)
}

## ----loop-model, message=TRUE-------------------------------------------------
fits_and_data <- loop_model(space_coord = nm_county_fips_2010, 
                            time_coord = year_coord, 
                            data_for_model = NM_popcas, 
                            outcome_col = "count", 
                            path_to_model = glm_func, 
                            use_cache = FALSE, 
                            min_train = 7)
all_fits <- fits_and_data[[1]]
all_data <- fits_and_data[[2]]

## ----loop-extract-yhat, message = TRUE----------------------------------------
all_yhats <- loop_extract_yhat(space_coord = nm_county_fips_2010,
                               time_coord = year_coord,
                               list_of_model_fits = all_fits,
                               list_of_model_data = all_data,
                               yhat_extractor_name = "extract",
                               use_surveillance_residuals = FALSE,
                               use_cache = FALSE)

## ----loop-alarm-function, message = TRUE--------------------------------------
all_alarms <- loop_alarm_function(space_coord = nm_county_fips_2010,
                                  time_coord = year_coord,
                                  list_of_yhats = all_yhats,
                                  list_of_model_data = all_data,
                                  outcome_col = "count",
                                  alarm_function_name = "scan_eb_poisson",
                                  max_k = 10,
                                  n_mcsim = 25,
                                  use_cache = FALSE)

