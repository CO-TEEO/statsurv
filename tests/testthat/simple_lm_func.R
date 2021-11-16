model_function <- function(space_coord, time_coord, data_for_model) {
  fit <- lm(y ~ time,
            data = data_for_model)
  return(list(fit = fit,
              data = data_for_model))
}
