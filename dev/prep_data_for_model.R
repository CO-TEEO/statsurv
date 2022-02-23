prep_data_for_model <- function(curr_data, curr_time_index, n_predict, outcome_col, prediction_strategy) {
  mask <- curr_data$id_time > (curr_time_index - n_predict)

  if (prediction_strategy == "NA") {
    curr_data[mask, outcome_col] <- NA
  } else if (prediction_strategy == "truncate") {
    curr_data <- curr_data[!mask, ]
  } else {
    stop("Invalid value for 'prediction_strategy'")
  }
  return(curr_data)

}
