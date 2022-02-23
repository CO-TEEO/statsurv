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

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("CO-TEEO/statsurv")

## ----data-loading, message=FALSE----------------------------------------------
library(scanstatistics)
library(broom.mixed)
library(lubridate)
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library(broom)
library(lme4)
library(sf)


data("NM_county_sf")
data("NM_data")

NM_data <- as_tibble(NM_data) %>%
  select(-baseline_est) 
NM_data

## -----------------------------------------------------------------------------
prediction_data <- NM_data %>% 
  filter(year <= 1989)

training_data <- prepare_training_data(prediction_data,
                                         outcome_cols = count, 
                                         split_id = 14, 
                                         prep_strategy = "truncate")

## ----model-1------------------------------------------------------------------
mod <- glm(count ~ I(year - 1985),
           family = poisson(link = "log"),
           offset = log(population),
           data = training_data)

## ----extract-yhat-------------------------------------------------------------
aug_data <- extract_yhat(mod, prediction_data)
plot(aug_data$count, aug_data$.fitted, xlab = "Observed Counts", ylab = "Model Predictions")
abline(a = 0, b = 1, col = "blue")

## ----model-2------------------------------------------------------------------
mod2 <- glmer(count ~ I(year - 1985) + (1 | county) + offset(log(population)),
              family = poisson(link = "log"),
              data = training_data)

## ----extract-yhat-2-----------------------------------------------------------
aug_data2 <- extract_yhat(mod2, prediction_data)
plot(aug_data2$count, aug_data2$.fitted, xlab = "Observed Counts", ylab = "Model Predictions")
abline(a = 0, b = 1, col = "blue")

## -----------------------------------------------------------------------------
zones <- create_zones(NM_county_sf, max_k = 10, min_k = 1)

## -----------------------------------------------------------------------------
scan_res <- scan_eb_poisson2(aug_data2, outcome_col = count, zones, 
                             baseline_col = .fitted, n_mcsim = 99)
scan_res

## -----------------------------------------------------------------------------
NM_county_sf %>%
  filter(id_space %in% c(15, 26))

## -----------------------------------------------------------------------------
windowed_data <- window_idtime(NM_data, min_train = 7, max_train = Inf, n_predict = 3,
                               step = 1)
windowed_data

## -----------------------------------------------------------------------------
windowed_data %>%
  rowwise() %>%
  mutate(training_data = list(prepare_training_data(curr_data,
                                                      outcome_cols = count,
                                                      split_id = split_id,
                                                      prep_strategy = "truncate")),
         mod = list(glmer(count ~ I(year - 1985) + (1 | county) + offset(log(population)),
                       family = poisson(link = "log"),
                       data = training_data)))

## -----------------------------------------------------------------------------
model_fits <- windowed_data %>%
  rowmute(training_data = prepare_training_data(curr_data, 
                                                  outcome_col = count,
                                                  split_id = split_id, 
                                                  prep_strategy = "truncate"),
          mod =  glmer(count ~ I(year - 1985) + (1 | county) + offset(log(population)),
                       family = poisson(link = "log"),
                       data = training_data))

model_fits

## -----------------------------------------------------------------------------
model_predictions <- model_fits %>%
  rowmute(aug_data = extract_yhat(mod, curr_data))

## -----------------------------------------------------------------------------
scan_results <- model_predictions %>%
  rowmute(scan_res = scan_eb_poisson2(aug_data, outcome_col = count, zones, 
                             baseline_col = .fitted, n_mcsim = 99))
scan_results

## -----------------------------------------------------------------------------
top_clusters_over_time <- scan_results %>%
  rowmute(top_cluster = report_top_clusters(scan_res, score, max_reported = 1)) %>%
  select(window_time_id, top_cluster) %>%
  unnest(top_cluster)

## -----------------------------------------------------------------------------
years <- distinct(NM_data[, c("id_time", "year")])
top_clusters_over_time <- left_join(top_clusters_over_time, years, 
                                    by = c("window_time_id" = "id_time")) 
top_clusters_over_time %>%
  rowmute(counties = NM_county_sf$county[zones[[zone]]]) %>%
  select(year, duration, score, mc_pvalue, counties) %>%
  unnest_wider(counties)


## -----------------------------------------------------------------------------
trends_in_zone132 <- scan_results %>%
  rowmute(all_clusters = report_top_clusters(scan_res, score, max_reported = Inf)) %>%
  select(window_time_id, all_clusters) %>%
  unnest(all_clusters) %>%
  filter(zone == 132) %>%
  left_join(years, by = c("window_time_id" = "id_time")) 

ggplot(trends_in_zone132, aes(x = year, y = mc_pvalue)) + 
  geom_line() +
  geom_point()

## ----new-alarms---------------------------------------------------------------
# Use the negative binomial scan function instead of the expectation-based poisson
nb_scan_results <- model_predictions %>%
  rowmute(scan_res = scan_eb_negbin2(aug_data, outcome_col = count, zones, 
                                     baseline_col = .fitted, n_mcsim = 99,
                                     theta_col = 1))


## ----cusum--------------------------------------------------------------------
cusum_results <- model_predictions %>%
  rowmute(cumusm_res = parallel_cusum_poisson2(aug_data, count, .fitted))


## ----INLA---------------------------------------------------------------------
library(INLA)
model_fits <- windowed_data %>%
  rowmute(training_data = prepare_training_data(curr_data, 
                                                  outcome_col = count,
                                                  split_id = split_id, 
                                                  prep_strategy = "NA"),
          mod =  inla(count ~ I(year - 1985) + f(county, model = "iid"),
                      family = "poisson",
                      data = training_data,
                      offset = log(population),
                      control.predictor=list(compute = TRUE)))


## ----INLA-extract-yhat--------------------------------------------------------
model_fits %>%
  rowmute(aug_data = extract_yhat(mod, curr_data))


