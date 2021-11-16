model_extractor2 <- function(space_coord, time_coord, fit, data, scaling) {
  space_label = gridcoord::gc_get_name(space_coord)
  time_label <- gridcoord::gc_get_name(time_coord)

  yhat <- data.frame(v1 = data[[space_label]],
                     v2 = data[[time_label]],
                     v3 = stats::predict(fit, data, type = "response"),
                     v4 = stats::predict(fit, data, type = "response") * scaling,
                     stringsAsFactors = FALSE)
  colnames(yhat) <- c(space_label, time_label, "yhat", "yhat_scaled")
  return(yhat)
}
