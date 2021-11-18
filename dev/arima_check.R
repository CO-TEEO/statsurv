data_prep_arima <- function(data_for_model) {
    
    df <- data_for_model
    
    df$month_36 <- pmax(df$id_time - 36, 0)
    
    if(nrow(df) < 40){ 
        vectors <-  c("id_time") # remove month36 if it is just a column of all zeroes
    } else {
        vectors <- c("id_time","month_36")
    }
    external_regressor_matrix <- as.matrix(x = df[, vectors])
    colnames(external_regressor_matrix) <- vectors
    
    # # My ARIMA model apparently breaks if everything is equal to zone. 
    # if (sd(df$percent_elevated) == 0) {
    #     y <- df$percent_elevated
    #     y <- y + runif(length(y), min = 0, max = 2e-5)
    #     df$percent_elevated <- y
    # }
    return(list(data = df, xreg = external_regressor_matrix))
}

model_arima <- function(df, xreg) {
    Sys.sleep(0.1)
    fit_arima <- forecast::Arima(df$yp,
                                 order = c(1,1,0),
                                 method = "ML",
                                 include.mean = TRUE, # include intercept
                                 transform.pars = FALSE,
                                 xreg = xreg)
    return(fit_arima)
}

# Ok, this is what I want to make work. 
# The question is...how?

library(tidyverse)
library(forecast)
library(progressr)
id_time <- seq(1:100)
d <- rnorm(n = length(id_time))
y <- cumsum(d)
arima_data <- tibble::tibble(id_time = id_time, id_space = 1, y = y)
knot <- 42
arima_data <- arima_data %>%
    dplyr::mutate(yp = y + ifelse(id_time > knot, (id_time - knot) * 0.2, 0))


# handlers(global = TRUE)
fits_and_data <- loop_model(arima_data, "yp", model_arima, data_prep_arima, min_train = 20,
                            prediction_strategy = "truncate")
arima_res <- fits_and_data
arima_res[85, ]
arima_res$model_fit[[85]]$fitted

data_prep_arima2 <- function(data_for_model) {
    df <- data_for_model
    df$month_36 <- pmax(df$id_time - 36, 0)
    
    if(nrow(df) < 41){ 
        vectors <-  c("id_time") # remove month36 if it is just a column of all zeroes
    } else {
        vectors <- c("id_time","month_36")
    }
    external_regressor_matrix <- as.matrix(x = df[, vectors])
    colnames(external_regressor_matrix) <- vectors
    
    return(list(data = df, xreg = external_regressor_matrix))
}


arima_res2 <- loop_extract_yhat(arima_res, data_prep_arima2, yhat_extractor_name = "extract",
                  use_surveillance_residuals = FALSE)
