library(here)
# source(here("tests", "testthat", "setup-alarm_function_data.R"))

test_that("pivot_for_scan works", {
  res <- pivot_for_scan(spacetime_data_lg, cases)
  expect_equal(unname(res), wide_cases_lg)
  expect_equal(colnames(res), paste(1:9))
  expect_equal(rownames(res), paste(1:4))
})

test_that("Error if non-unique", {
  non_unique_df <- spacetime_data_lg
  non_unique_df[2, "id_space"] <- 1
  expect_error(pivot_for_scan(non_unique_df, cases), class = "error_bad_spacetimedata")
})


test_that("Pads with NAs if incomplete", {
  incomplete_df <- spacetime_data_lg[1:34, ]
  res <- pivot_for_scan(incomplete_df, cases)
  expected2 <- wide_cases_lg
  expected2[4, 8:9] <- NA
  expect_equal(unname(res), expected2)
  expect_equal(colnames(res), paste(1:9))
  expect_equal(rownames(res), paste(1:4))
})


test_that("Order of the rows doesn't matter", {
  res <- pivot_for_scan(spacetime_data_lg, cases)
  for (i in 1:10) {
    res2 <- pivot_for_scan(dplyr::slice_sample(spacetime_data_lg, prop = 1), cases)
    expect_equal(res, res2)
  }
})

test_that("We can do this with any column", {
 res <- pivot_for_scan(spacetime_data_lg, cases)
 expect_equal(unname(res), wide_cases_lg)

 res <- pivot_for_scan(spacetime_data_lg, .fitted)
 expect_equal(unname(res), wide_baseline_lg)

 res <- pivot_for_scan(spacetime_data_lg, pop)
 expect_equal(unname(res), wide_pop_lg)

 res <- pivot_for_scan(spacetime_data_lg, probs)
 expect_equal(unname(res), wide_probs_lg)

 res <- pivot_for_scan(spacetime_data_lg, thetas)
 expect_equal(unname(res), wide_thetas_lg)

 res <- pivot_for_scan(spacetime_data_lg, id_space)
 expected <- matrix(1:9, nrow = 4, ncol = 9, byrow = TRUE)
 expect_equal(unname(res), expected)

 res <- pivot_for_scan(spacetime_data_lg, id_time)
 expected <- matrix(1:4, nrow = 4, ncol = 9, byrow = FALSE)
 expect_equal(unname(res), expected)
})
