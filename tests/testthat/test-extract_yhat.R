
test_that("sample + collapse_yhat tends towards extract_yhat", {
  fit_lm <- lm(formulas$f_lm,
               data = ey_data)
  yhat_extract <- extract_yhat(ey_space, ey_time, fit_lm, ey_data)
  yhat_sample <- sample_yhat(ey_space, ey_time, fit_lm, ey_data, n_samples = 1000)
  yhat_collapse <- collapse_yhat(space_coord, time_coord, yhat_sample)
  expect_equal(yhat_extract, yhat_collapse, tolerance = 0.01)
})

test_that("parse_yhat_extractor works", {
  expect_equal(parse_yhat_extractor("extract"), extract_yhat)
  expect_equal(parse_yhat_extractor("sample"), sample_yhat)
  expect_equal(parse_yhat_extractor(NULL), NULL)
  source("model_extractor.R")
  expect_equal(parse_yhat_extractor("model_extractor.R"),
               model_extractor)
  expect_error(parse_yhat_extractor("bananasplit.R"),
               "Unable")
})

test_that("validate yhat works", {
  space_coord <- data.frame(space = c("A", "B"),
                            stringsAsFactors = FALSE)
  time_coord <- data.frame(time = c("t1", "t2", "t3"),
                           stringsAsFactors = FALSE)
  data <- gridcoord::gc_expand(space_coord, time_coord)
  data$yhat <- 1:6
  expect_error(validate_yhat(space_coord, time_coord, data),
               NA)
  bad_data <- data[, c(2, 3)]
  expect_error(validate_yhat(space_coord, time_coord, bad_data),
               "coordinates")
  bad_data <- data[, c(1, 3)]
  expect_error(validate_yhat(space_coord, time_coord, bad_data),
               "coordinates")
  bad_data <- data[, c(1, 2)]
  expect_error(validate_yhat(space_coord, time_coord, bad_data),
               "predicted")
  ok_data <- data
  ok_data$yhat2 <- ok_data$yhat + 3
  expect_error(validate_yhat(space_coord, time_coord, ok_data),
               NA)
  expect_error(validate_yhat(space_coord, time_coord, c(1:6)))

})
