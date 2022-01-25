library(tidyverse)

# We're going to set up a very basic model and then try to break it
space_coord <- data.frame(space_label = c("space1", "space2", "space3"),
                          stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()



start_times <- 1:12
fin_times <- start_times + 1L
labels <- paste0("X", start_times) #sapply(start_times + 64, intToUtf8)
time_coord <- data.frame(time_labels = labels,
                         time = start_times,
                         fin_time = fin_times,
                         stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()
data_for_model <- gridcoord::gc_expand(time_coord, space_coord)
data_for_model$y <- data_for_model$time * 1.05 + rnorm(n = nrow(data_for_model))

spacetime_data <- data_for_model %>%
  mutate(id_space = as.numeric(factor(space_label)),
         id_time = time) %>%
  as_tibble() %>%
  select(id_space, id_time, time, fin_time, y)

spacetime_data$id_time

# That works, but feels a little...fragile
# I feel like I would need to put in a way to make sure that the index is complete and ordered
# But otherwise, that does the major windowing that I want to do
simple_lm_func <- function(data_for_model) {
  Sys.sleep(1)
  fit <- lm(y ~ time,
            data = data_for_model)
  return(fit)
}
library(progressr)
handlers(global = TRUE)

lm_res <- loop_model(spacetime_data,
           "y",
           simple_lm_func,
           data_prep_function = NULL,
           min_train = 7,
           max_train = 10,
           n_predict = 1,
           model_arity = "uni",
           prediction_strategy = "NA")


lm_yhat <- loop_extract_yhat(lm_res,"extract",  NULL, include_surveillance = TRUE)
lm_yhat$augmented_data

lm_yhat$surveillance_data

loop_alarm_function(lm_yhat, "y", "parallel_cusum_gaussian")
