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

## ----displaying-results-------------------------------------------------------
all_coeffs <- list()
for (ind in seq_len(length(all_fits))) {
  if (identical(all_fits[[ind]], NA)) {
    next
  }
    curr_label <- names(all_fits)[[ind]]
    curr_df <- report_model_coeff(all_fits[[ind]])
    curr_df[["year"]] <- as.numeric(names(all_fits)[[ind]])
    all_coeffs[[ind]] <- curr_df
}
tall_coeff_df <- do.call(rbind, all_coeffs)
library("ggplot2")
ggplot(tall_coeff_df, aes(x = year, y = estimate, color = term, group = term)) +
  geom_point() +
  geom_line() +
  facet_wrap("term", scales = "free")

## ----extract-clsuters---------------------------------------------------------

mlc <- extract_clusters(all_alarms[["1989"]], zones, max_allowed = 1)
print(mlc)
nm_county_fips_2010[zones[[mlc$zone]], ]

## ----extract-stat-------------------------------------------------------------
alarm_df <- extract_alarm_statistic(nm_county_fips_2010, year_coord, all_alarms, mlc$zone)
ggplot(alarm_df, aes(x = surveillance_date, y = action_level, color = action_level)) + 
  geom_point() + 
  geom_line() + 
  geom_line(aes(y = upper_control_limit), color = "gray30", linetype = "dashed")


## ----new-alarms---------------------------------------------------------------
# Use the negative binomial scan function instead of the expectation-based poisson
all_alarms <- loop_alarm_function(space_coord = nm_county_fips_2010,
                                  time_coord = year_coord,
                                  list_of_yhats = all_yhats,
                                  list_of_model_data = all_data,
                                  outcome_col = "count",
                                  alarm_function_name = "scan_eb_negbin_fast",
                                  max_k = 10,
                                  n_mcsim = 25,
                                  use_cache = FALSE)


## ----cusum--------------------------------------------------------------------
all_alarms_x150 <- loop_alarm_function(space_coord = nm_county_fips_2010,
                                  time_coord = year_coord,
                                  list_of_yhats = all_yhats,
                                  list_of_model_data = all_data,
                                  outcome_col = "count",
                                  alarm_function_name = "parallel_cusum_poisson",
                                  max_k = 10,
                                  n_mcsim = 25,
                                  use_cache = FALSE)
all_alarms_x150[["1991"]][1:10, 1:10]

all_alarms_x125 <- loop_alarm_function(space_coord = nm_county_fips_2010,
                                  time_coord = year_coord,
                                  list_of_yhats = all_yhats,
                                  list_of_model_data = all_data,
                                  outcome_col = "count",
                                  alarm_function_name = "parallel_cusum_poisson",
                                  max_k = 10,
                                  n_mcsim = 25,
                                  use_cache = FALSE,
                                  extra_alarm_args = list(scaling = 1.25))
plot_df1 <- as.data.frame(all_alarms_x150[["1991"]]) %>%
  tibble::rownames_to_column(var = "year") %>%
  tidyr::pivot_longer(cols = -year, values_to = "alarm_statistic") %>%
  mutate(scaling = 1.5)

plot_df2 <- as.data.frame(all_alarms_x125[["1991"]]) %>%
  tibble::rownames_to_column(var = "year") %>%
  tidyr::pivot_longer(cols = -year, values_to = "alarm_statistic") %>%
  mutate(scaling = 1.25)
plot_df <- rbind(plot_df1, plot_df2)
ggplot(dplyr::filter(plot_df, name == "santafe"),
       aes(x = as.numeric(year), y = alarm_statistic, color = factor(scaling), group = scaling)) +
         geom_point() +
  geom_line() +
  ggtitle("CUSUM statistic in Santa Fe")

## ---- eval = FALSE------------------------------------------------------------
#  extra_alarm_args = list(scaling = 1.25))

## ----model-inla---------------------------------------------------------------
model_inla_bym <- function(space_coord, time_coord, data_for_model) {
  library(INLA)
  
  neighbors <- spdep::poly2nb(space_coord, queen = FALSE)
  graph_file <-  tempfile()
  spdep::nb2INLA(graph_file, neighbors)

  f <- count ~ year +  
    f(fips_id,
      model = "bym",
      graph = graph_file,
      hyper = list(prec.unstruct = list(prior = "loggamma",
                                        param = c(3.2761, 1.81)),
                   prec.spatial = list(prior = "loggamma",
                                       param = c(1, 1))))
  fit_inla <- inla(formula = f,
                        family = "poisson",
                        data = data_for_model,
                        control.family = list(link = "log"),
                        control.compute = list(config = TRUE,
                                               dic = TRUE),
                        control.predictor = list(compute=TRUE,
                                                 link = 1),
                        offset = offset)
  
                        # control.fixed = list(mean = 0,
                        #                      prec = 0.1)
  return(list(fit = fit_inla,
              data = data_for_model))
}


## ---- message = TRUE----------------------------------------------------------
NM_popcas$fips_id <- gridcoord::gc_get_match(NM_popcas, nm_county_fips_2010)
fits_and_data <- loop_model(space_coord = nm_county_fips_2010, 
                            time_coord = year_coord, 
                            data_for_model = NM_popcas, 
                            outcome_col = "count", 
                            path_to_model = model_inla_bym, 
                            use_cache = FALSE, 
                            min_train = 7,
                            n_predict = 1)
all_fits_inla <- fits_and_data[[1]]
all_data_inla <- fits_and_data[[2]]

all_yhat_inla <- loop_extract_yhat(space_coord = nm_county_fips_2010,
                               time_coord = year_coord,
                               list_of_model_fits = all_fits_inla,
                               list_of_model_data = all_data_inla,
                               yhat_extractor_name = "extract",
                               use_surveillance_residuals = FALSE,
                               use_cache = FALSE)

all_alarms_inla <- loop_alarm_function(space_coord = nm_county_fips_2010,
                                       time_coord = year_coord,
                                       list_of_yhats = all_yhat_inla,
                                       list_of_model_data = all_data_inla,
                                       outcome_col = "count",
                                       alarm_function_name = "scan_eb_poisson",
                                       max_k = 10,
                                       n_mcsim = 25,
                                       use_cache = FALSE)


## ----displaying-results-inla--------------------------------------------------
all_coeffs <- list()
for (ind in seq_len(length(all_fits_inla))) {
  if (identical(all_fits[[ind]], NA)) {
    next
  }
    curr_label <- names(all_fits)[[ind]]
    curr_df <- report_model_coeff(all_fits_inla[[ind]])
    curr_df[["year"]] <- as.numeric(names(all_fits_inla)[[ind]])
    all_coeffs[[ind]] <- curr_df
}
tall_coeff_df <- do.call(rbind, all_coeffs)
library("ggplot2")
ggplot(tall_coeff_df, aes(x = year, y = estimate, color = term, group = term)) +
  geom_point() +
  geom_line() +
  facet_wrap("term", scales = "free")

## ----extract-clsuters-inla----------------------------------------------------
mlc <- extract_clusters(all_alarms_inla[["1989"]], zones, max_allowed = 1)
print(mlc)
nm_county_fips_2010[zones[[mlc$zone]], ]

## ----extract-stat-inla--------------------------------------------------------
alarm_df <- extract_alarm_statistic(nm_county_fips_2010, year_coord, all_alarms_inla, mlc$zone)
ggplot(alarm_df, aes(x = surveillance_date, y = action_level, color = action_level)) + 
  geom_point() + 
  geom_line() + 
  geom_line(aes(y = upper_control_limit), color = "gray30", linetype = "dashed")

