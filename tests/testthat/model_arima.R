model_arima <- function(space_coord, time_coord, data_for_model, n_ahead = 1) {
    library("forecast")
    
    df <- data_for_model
    
    external_regressor_matrix <- as.matrix(x = df[, c("offset", "months_since_14", "x")])

    # perform a standardized ARIMA model on the data (variations from SAS are negligible)
    rows_to_hold <- seq(nrow(external_regressor_matrix) - n_ahead + 1,
                        nrow(external_regressor_matrix))
    
    fit_arima <- forecast::Arima(df$y[-rows_to_hold],  # don't model on last n rows
                       order = c(1,0,0),
                       method = "ML",
                       include.mean = FALSE, 
                       transform.pars = FALSE,
                       xreg = external_regressor_matrix[-rows_to_hold, , drop = FALSE] # don't model on last n rows
    )
    return(list(fit = fit_arima, data = df, xreg = external_regressor_matrix))
}

