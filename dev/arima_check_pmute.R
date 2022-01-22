data_prep_arima <- function(data_for_model) {

    df <- data_for_model

    df$month_36 <- pmax(df$id_time - 36, 0)

    return(df)
}

make_xreg <- function(data_for_model) {
    if(nrow(data_for_model) < 40){
        vectors <-  c("id_time") # remove month36 if it is just a column of all zeroes
    } else {
        vectors <- c("id_time","month_36")
    }
    xreg <- as.matrix(x = data_for_model[, vectors])
    colnames(xreg) <- vectors
    return(xreg)
}


make_xreg2 <- function(data_for_model) {

    if(nrow(data_for_model) < 41){
        vectors <-  c("id_time") # remove month36 if it is just a column of all zeroes
    } else {
        vectors <- c("id_time","month_36")
    }
    xreg <- as.matrix(x = data_for_model[, vectors])
    colnames(xreg) <- vectors
    return(xreg)
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


nested_arima_data <- window_spacetime(arima_data, min_train = 20,
                                      max_train = Inf, n_predict = 1, model_arity = "uni")
prepped_arima_data <- nested_arima_data %>%
    pmute(curr_data = mutate(curr_data, month_36 = pmax(id_time - 36, 0)),
          data_for_model = prep_data_for_model(curr_data, id_time, n_predict = 1,
                                               outcome_col = "yp",prediction_strategy = "truncate"),
          xreg = make_xreg(data_for_model))

handlers(global = TRUE)
model_fits <- prepped_arima_data %>%
    pmute(fit = model_arima(data_for_model, xreg))

model_yhats <- model_fits %>%
    pmute(xreg2 = make_xreg2(curr_data),
          aug_data = extract_yhat(fit, curr_data, xreg2))



