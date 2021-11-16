## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(statsurv)

## ----eval = FALSE-------------------------------------------------------------
#  model_lm <- function(space_coord, time_coord, data_for_model) {
#    formula <- y ~ x
#    fit_obj < - lm(formula,
#                   data = data_for_model)
#    return(list(fit = fit_obj,
#                data = data_for_model))
#  }

## ---- eval = FALSE------------------------------------------------------------
#  library("MASS")
#  mu_est <- mean(data_for_model$y, na.rm = TRUE)
#  var_est <- sd(data_for_model$y, na.rm = TRUE) ^ 2
#  # For a n-binom model, variance = mu + (mu ^ 2) / theta
#  theta_est <- mu_est ^ 2 / (var_est - mu_est)
#  fit <- glm.nb(y ~ 1,
#                data = data_for_model,
#                init.theta = theta_est)

## ---- eval = FALSE------------------------------------------------------------
#  model_glm_nb <- function(time_coord, space_coord, data_for_model) {
#    library("MASS")
#    mu_est <- mean(data_for_model$y, na.rm = TRUE)
#    var_est <- sd(data_for_model$y, na.rm = TRUE) ^ 2
#    theta_est <- mu_est ^ 2 / (var_est - mu_est)
#    fit <- glm.nb(y ~ 1,
#                  data = data_for_model,
#                  init.theta = theta_est)
#    return(list(fit = fit, data = data_for_model))
#  }

## ---- eval = FALSE------------------------------------------------------------
#  model_arima <- function(time_coord, space_coord, data_for_model, n_ahead) {
#    #...
#  }

## ---- eval = FALSE------------------------------------------------------------
#  loop_model(space_coord, time_coord, data_for_model, outcome_col = "y",
#             path_to_model = model_arima, n_predict = 3,
#             extra_model_args = list(n_ahead = 3))

## ---- eval = FALSE------------------------------------------------------------
#  model_ranger <- function(time_coord, space_coord, data_for_model, n_trees) {
#    library("ranger")
#    fit <- ranger(y ~ .,
#                  data = data_for_model,
#                  num.trees = n_trees)
#    return(list(fit = fit, data = data_for_model))
#  }
#  
#  loop_model(space_coord, time_coord, data_for_model, outcome_col = "y",
#             path_to_model = model_arima, n_predict = 3,
#             extra_model_args = list(n_trees = 300))

## ---- eval = FALSE------------------------------------------------------------
#  model_ranger2 <- function(time_coord, space_coord, data_for_model) {
#    n_trees <- data_for_model$n_trees[[1]]
#    fit <- ranger(y ~ .,
#                  data = data_for_model,
#                  num.trees = n_trees)
#    return(list(fit = fit, data = data_for_model))
#  }
#  
#  data_for_model$n_trees <- 302
#  loop_model(space_coord, time_coord, data_for_model, outcome_col = "y",
#             path_to_model = model_arima, n_predict = 3)

## ---- eval = FALSE------------------------------------------------------------
#  model_arima <- function(time_coord, space_coord, data_for_model, .n_ahead) {
#    library("forecast")
#    df <- data_for_model
#    df$index <- 1:nrow(df)
#    df$t_36 <- pmax(df$index - 36, 0)
#  
#    xreg <- as.matrix(df[, c("covar1", "t_36")])
#    n <- nrow(df)
#    ycol <- df$percent_elevated
#    ###>>> Key part for removing values
#    rows_for_predicting <- seq(n - .nahead + 1, n)
#    xreg_for_fitting <- xreg[-rows_for_predicting, ]
#    ycol_for_fitting <- ycol[-rows_for_predicting]
#    ###<<<
#  
#    fit_arima <- forecast::Arima(ycol_for_fitting
#                         order = c(1,0,0),
#                         method = "ML",
#                         include.mean = TRUE, # include intercept
#                         transform.pars = FALSE,
#                         xreg = xreg_for_fitting
#      )
#    return(list(fit = fit_arima,
#                data = df,
#                xreg = xreg))
#  }
#  

## ---- eval = FALSE, background = "red"----------------------------------------
#  model_glm <- function(time_coord, space_coord, data_for_model) {
#    offset_data <- log(data_for_model$total_population)
#    fit_glm <- glm(total_elevated ~ 1 + years_since_2006,
#                   data = data_for_model,
#                   family = poisson(link = "log"),
#                   offset = offset_data)
#    return(list(fit = fit_glm,
#                data = data_for_model))
#  }
#  

## ---- eval = FALSE, background = "green"--------------------------------------
#  model_glm <- function(time_coord, space_coord, data_for_model) {
#    fit_glm <- glm(total_elevated ~ 1 + years_since_2006,
#                   data = data_for_model,
#                   family = poisson(link = "log"),
#                   offset = log(total_population) )
#    return(list(fit = fit_glm,
#                data = data_for_model))
#  }
#  

