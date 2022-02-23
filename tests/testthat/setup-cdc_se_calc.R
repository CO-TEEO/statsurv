cdc_se_calc <- function(design_matrix, cov_matrix_of_model, MSE, AR_coeff, remove_AR_from_cov_matrix = TRUE){
    #' this function calculates the standard error of each prediction ~y~(t) for identifying
    #' a confidence interval around the prediction.
    #' using the SAS formula:
    #'  se(t) = sqrt ( Z[t] [matrixmultiply] Vb [matrixmultiply] Z[t]'  + s^2r  )
    #'  where Z[t] is the t'th row of the design matrix + the Ar_coeff * [t-1]'th row
    #'   Vb is the variance-covariance matrix of the model parameters (excluding the AR parameter phi)
    #'   s^2 is the Mean Square Error (i.e. Sum of square error / degrees of freedom )
    #'  and r is the conditional prediction error variance multiplier for s^2
    #'  essentially, if the errors (epsilon) of the autoregressive component are correlated
    #'  r != 1; but the assumption of the AR(1) model:
    #'  y = [x][b] + v
    #'  v = [phi][y[t-1]] + epsilon[t]
    #'  is that epsilon is iid normally distributed with variance sigma^2 approximated by s^2.
    #' so we assume r is 1, and that we are just adding s^2

    d = design_matrix

    c = cov_matrix_of_model

    if(remove_AR_from_cov_matrix == TRUE){
        # the variance covariance matrix is likely a 7x7 matrix,
        # but the design matrix doesn't have the ar column
        c = c[-1,-1]
    }

    m = MSE
    a = AR_coeff

    r = rep(NA, nrow(d))
    r[1] <- sqrt(
        # as.matrix makes it a column, need to transpose to be a 1 x N
        # c is an N x N matrix
        # 1 x N   N x N    N x 1   ==>  1 x 1 matrix    + scalar
        # 0 value for the phi addition on the first loop.
        as.numeric(
            t ( as.matrix(d[1,]) + 0) %*% c %*% (as.matrix(d[1,]) + 0) + m
        )
    )

    for(i in 2:length(r)){
        r[i] <- sqrt(
            as.numeric(
                #' as defined above
                #' Zt = Xt + phiXt
                #'
                t ( as.matrix(d[i,]) + (a*as.matrix(d[i-1,])) ) %*% c %*% (as.matrix(d[i,]) + (a*as.matrix(d[i-1,])) ) + m

            )
        )
    }
    return(r)
}


compute_arima_se <- function(arima_fit, add_intercept = TRUE) {
    if (!is.logical(add_intercept) || length(add_intercept) != 1) {
        stop("add_intercept must be either TRUE or FALSE")
    }
    residuals <- arima_fit$residuals
    d_o_f <- nrow(arima_fit$xreg) - ncol(arima_fit$xreg) - 2
    # df = number of rows - the vectors_ parameters, - 3 {intercept, AR(1)}
    MSE <-  (sum(residuals^2) / d_o_f)
    # the design matrix needs to have the intercept column put in manually
    # as the arima model inputs an intercept with the "include_mean = TRUE" parameter
    if (add_intercept) {
        design_matrix = data.frame(intercept = rep(1 ,nrow(arima_fit$xreg)))
        design_matrix = cbind(design_matrix, arima_fit$xreg)
    } else {
        design_matrix <- arima_fit$xreg
    }
    design_matrix <- as.matrix(design_matrix) # must be class matrix to proceed.

    # get the variance-covariance matrix of the model, excluding the AR() term
    cov_matrix = arima_fit$var.coef
    AR_coeff = -as.numeric(arima_fit$coef[[1]]) #sign flipped to match SAS

    pred_ses <- cdc_se_calc(design_matrix = design_matrix,
                            cov_matrix_of_model = cov_matrix,
                            MSE = MSE, AR_coeff = AR_coeff, remove_AR_from_cov_matrix = TRUE)

    v <- pred_ses ^ 2
    return(v)
}

compute_arima_se <<- compute_arima_se

