devtools::load_all()

set.seed(210321060)
op <- options(simplecache.debug = TRUE)

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

simple_lm_func <- function(space_coord, time_coord, data_for_model) {
  fit <- lm(y ~ time,
            data = data_for_model)
  return(list(fit = fit,
              data = data_for_model))
}

# Ok, the easy way to get the info we want would be to call loop_model.
# But that makes it harder to de-bug, b/c then we don't know if we made a change, is it coming from loop_extract_yhat or loop_model?
# BUt I think that's why we have tests for both.
simple_lm_output <- loop_model(space_coord, time_coord, data_for_model, "y",
                               path_to_model = simple_lm_func,
                               min_train = 7,
                               use_cache = FALSE,
                               verbose = FALSE)
simple_lm_output_exploded <- loop_model(space_coord, time_coord, data_for_model, "y",
                                        path_to_model = simple_lm_func,
                                        model_arity = "uni",
                                        min_train = 7,
                                        use_cache = FALSE,
                                        verbose = FALSE)

for(i in 1:100) {
  all_yhat <- loop_extract_yhat(space_coord, time_coord,
                                simple_lm_output[[1]], simple_lm_output[[2]],
                                path_to_model = simple_lm_func,
                                use_cache = FALSE,
                                verbose = FALSE)
}
