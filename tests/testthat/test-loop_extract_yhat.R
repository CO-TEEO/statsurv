set.seed(210321060)

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
  dplyr::mutate(id_space = as.numeric(factor(space_label)),
                id_time = time) %>%
  tibble::as_tibble() %>%
  dplyr::select(id_space, id_time, time, fin_time, y)

simple_lm_func <- function(data_for_model) {
  fit <- lm(y ~ time,
            data = data_for_model)
  return(fit)
}

# Ok, the easy way to get the info we want would be to call loop_model.
# But that makes it harder to de-bug, b/c then we don't know if we made a change, is it coming from loop_extract_yhat or loop_model?
# BUt I think that's why we have tests for both.

simple_lm_output <-   loop_model(spacetime_data, "y", simple_lm_func,
                                 model_arity = "multi",
                                 min_train = 7)

simple_lm_output_exploded <- loop_model(spacetime_data, "y", simple_lm_func,
                                        model_arity = "uni",
                                        min_train = 7)

# just_coord_data <- function(space_coord, time_coord, all_fits, all_data) {
#   loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
#                     path_to_model = simple_lm_func,
#                     use_cache = FALSE,
#                     verbose = FALSE)
# }


# loop_extract_yhat <- function(fits_and_data,
#                               data_prep_function = NULL,
#                               yhat_extractor_name = "extract",
#                               include_surveillance = TRUE,
#                               grow_surveillance_length = TRUE,
#                               n_samples = NULL,
#                               ...) {

test_that("loop_extract_yhat behaves consistently", {
  all_yhat <- loop_extract_yhat(simple_lm_output, "extract", NULL)

  expect_known_value(all_yhat, file = "simple_lm_yhat.RDS")
})

test_that("loop_extract_yhat behaves consistently when exploded", {
  all_yhat <- loop_extract_yhat(simple_lm_output_exploded, "extract")
  expect_known_value(all_yhat, file = "simple_lm_yhat_exploded.RDS")
})



test_that("Still works if we only have one model fit", {
  test_data <- simple_lm_output[1:7, ]

  all_yhat <- loop_extract_yhat(test_data)
  expect_true("data.frame" %in% class(all_yhat))
  expect_true("data.frame" %in% class(all_yhat$surveillance_data[[7]]))
})


test_that("What we get out matches what we put in", {
  # Matches for extract, multi-variate
  all_yhat <- loop_extract_yhat(simple_lm_output)
  expect_equal(class(all_yhat), class(simple_lm_output))
  expect_equal(nrow(all_yhat), nrow(simple_lm_output))

  # And for sample, multi-variate
  all_yhat <- loop_extract_yhat(simple_lm_output, "sample")
  expect_equal(class(all_yhat), class(simple_lm_output))
  expect_equal(nrow(all_yhat), nrow(simple_lm_output))

  # And for extract, exploded
  all_yhat <- loop_extract_yhat(simple_lm_output_exploded)
  expect_equal(class(all_yhat), class(simple_lm_output_exploded))
  expect_equal(nrow(all_yhat), nrow(simple_lm_output_exploded))


  # And finally for sample, exploded
  all_yhat <- loop_extract_yhat(simple_lm_output_exploded, "sample")
  expect_equal(class(all_yhat), class(simple_lm_output_exploded))
  expect_equal(nrow(all_yhat), nrow(simple_lm_output_exploded))
})



test_that("We can supply our own extractor functions", {
  model_extractor <- function(fit, data, ...) {
    data$.fitted <- stats::predict(fit, newdata = data, type = "response") * 1
    return(data)
  }


  all_yhat <- loop_extract_yhat(simple_lm_output, model_extractor)
  all_yhat_orig <- loop_extract_yhat(simple_lm_output, "extract")
  expect_equal(all_yhat, all_yhat_orig)


  model_extractor2 <- function(fit, data, scaling) {

    data$.fitted <- stats::predict(fit, data, type = "response")
    data$.fitted_scaled <- stats::predict(fit, data, type = "response") * scaling
    return(data)
  }

  all_yhat_v2 <- loop_extract_yhat(simple_lm_output, model_extractor2, scaling = 3)

  test_aug <- all_yhat_v2$augmented_data[[9]]
  expect_true(all(round(test_aug$.fitted_scaled/test_aug$.fitted, 6) == 3))
})
