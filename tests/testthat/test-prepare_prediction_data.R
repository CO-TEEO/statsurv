set.seed(74837155)

x = rnorm(100)
spacetime_data <- data.frame(id_space = rep(1:10, each = 10),
                             id_time = rep(1:10, 10),
                             x = x,
                             y = 2.04 * x + 1.23)
spacetime_data$f <- rep(c(min, max, median, mean), each = 25)
spacetime_data$df <- data.frame(xx = 1:100, yy = 201:300)
spacetime_data$l <- rep(list(1:5), 100)
# organize_df <- function(df) {
#   df %>%
#     dplyr::arrange(dplyr::across()) %>%
#     as.data.frame() %>%
#     tibble::remove_rownames()
# }

# Need to check bad args.

test_that("Gives errors on bad inputs", {
  bad_df <- dplyr::select(spacetime_data, -id_time)
  expect_error(prepare_prediction_data(bad_df, y, split_id = 9))
  expect_error(prepare_prediction_data(spacetime_data, z, split_id = 9))
  expect_error(prepare_prediction_data(spacetime_data, FALSE, split_id = 9))

  expect_error(prepare_prediction_data(spacetime_data, y, split_id = 2.5))
  expect_error(prepare_prediction_data(spacetime_data, y, split_id = "a"))
  expect_error(prepare_prediction_data(spacetime_data, y, split_id = 14))
  expect_error(prepare_prediction_data(spacetime_data, y, split_id = -1))
  expect_error(prepare_prediction_data(spacetime_data, y, split_id = 9, prep_strategy = 2))
  expect_error(prepare_prediction_data(spacetime_data, y, split_id = 9, prep_strategy = "bad"))

  # Check for non-scalar inputs
  expect_error(prepare_prediction_data(spacetime_data, y, split_id = c(4, 5)))
  expect_error(prepare_prediction_data(spacetime_data, y, split_id = 3, prep_strategy = c("truncate", "NA")))

})

test_that("Single column selection works for NA", {
  for (ii in 1:10) {
    split_id <- sample(1:10, size = 1)
    res <- prepare_prediction_data(spacetime_data, y, split_id = split_id)
    expected <- spacetime_data
    expected[expected$id_time >= split_id, "y"] <- NA
    expect_equal(res, expected)
  }
})

test_that("Multiple column selection works for NA", {
  for (ii in 1:10) {
    split_id <- sample(1:10, size = 1)
    res <- prepare_prediction_data(spacetime_data, c(x, y), split_id = split_id)
    expected <- spacetime_data
    expected[expected$id_time >= split_id, c("x", "y")] <- NA
    expect_equal(res, expected)
  }
})

test_that("Single column selection works for truncate", {
  for (ii in 1:10) {
    split_id <- sample(1:10, size = 1)
    res <- prepare_prediction_data(spacetime_data, y, split_id = split_id, prep_strategy = "truncate")
    expected <- dplyr::filter(spacetime_data, id_time < split_id)
    expect_equivalent(res, expected) #Rownames on the data.frame columns were messing this up, I decided I didn't care
  }
})

test_that("Multiple column selection works for truncate", {
  for (ii in 1:10) {
    split_id <- sample(1:10, size = 1)
    res <- prepare_prediction_data(spacetime_data, c(x, y), split_id = split_id, prep_strategy = "truncate")
    expected <- dplyr::filter(spacetime_data, id_time < split_id)
    expect_equivalent(res, expected)
  }
})

test_that("Weird column selections work", {
  # Compare weird selection against a more normal selection that we know works.
  res <- prepare_prediction_data(spacetime_data, x:df, split_id = 9)
  expected <- prepare_prediction_data(spacetime_data, c(x, y, f, df), split_id = 9)
  expect_equal(res, expected)
  res <- prepare_prediction_data(spacetime_data, x:l, split_id = 9)
  expected <- prepare_prediction_data(spacetime_data, c(x, y, f, df, l), split_id = 9)
  expect_equal(res, expected)
  res <- prepare_prediction_data(spacetime_data, !(id_space:id_time), split_id = 9)
  expected <- prepare_prediction_data(spacetime_data, c(x, y, f, df, l), split_id = 9)
  expect_equal(res, expected)
  res <- prepare_prediction_data(spacetime_data, dplyr::last_col(), split_id = 9)
  expected <- prepare_prediction_data(spacetime_data, l, split_id = 9)
  expect_equal(res, expected)
  res <- prepare_prediction_data(spacetime_data, dplyr::starts_with("x") | dplyr::starts_with("y"),
                                 split_id = 9)
  expected <- prepare_prediction_data(spacetime_data, c(x, y), split_id = 9)
  expect_equal(res, expected)
})
