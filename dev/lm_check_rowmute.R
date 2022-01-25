library(tidyverse)
library(progressr)

# We're going to set up a very basic model and then try to break it
spacetime_data <- expand.grid(id_time = 1:12, id_space = 1:3) %>%
  mutate(time_labels = paste0("X", id_time),
         t = id_time,
         y = t * 1.05 + rnorm(n = max(row_number())))


# That works, but feels a little...fragile
# I feel like I would need to put in a way to make sure that the index is complete and ordered
# But otherwise, that does the major windowing that I want to do
simple_lm_func <- function(data_for_model) {
  Sys.sleep(1)
  fit <- lm(y ~ t,
            data = data_for_model)
  return(fit)
}

handlers(global = TRUE)
windowed_data <- window_idtime(spacetime_data, min_train = 7, max_train = 10,
                               n_predict = 1, model_arity = "uni")
prepped_data <- windowed_data %>%
  rowmute(input_data = prepare_prediction_data(curr_data, y, split_id = split_id, prep_strategy = "NA"))

lm_res <- prepped_data %>%
  rowmute(fits = simple_lm_func(input_data))

lm_yhat <- lm_res %>%
  rowmute(aug_data = extract_yhat(fits, curr_data))

collapsed_data <- collapse_if_exploded(lm_yhat)

alarm_res <- collapsed_data %>%
  rowmute(alarm = parallel_cusum_gaussian2(aug_data, "y", ".fitted"))

