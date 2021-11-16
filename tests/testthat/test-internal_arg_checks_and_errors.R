test_that("error_type throws good errors", {
  bananas <- "fruit"
  expect_error(error_type(bananas, "numeric"),
               "bananas",
               class = "error_type")
  expect_error(error_type(bananas, "numeric", "pineapple"),
               "pineapple",
               class = "error_type")


  expect_error(check_type(bananas, "numeric"),
               "bananas",
               class = "error_type")

  expect_error(check_type(bananas, c("numeric", "data.frame")),
               "bananas",
               class = "error_type")

  expect_error(check_scalar_type(bananas, c("numeric", "data.frame")),
               "bananas",
               class = "error_type")
})

test_that("error_scalar throws good errors", {
  avocado <- c("One", "Two", "Three")
  expect_error(error_scalar(avocados),
               "avocado",
               class = "error_scalar")
  expect_error(error_scalar(avocados, "fir"),
               "fir",
               class = "error_scalar")

  expect_error(check_scalar(avocado),
               "avocado",
               class = "error_scalar")

  expect_error(check_scalar_type(avocado, "character"),
               "avocado",
               class = "error_scalar")
})

test_that("validate_data_for_yhat throws good errors", {
  space_coord <- data.frame(space = c("A", "B"),
                            stringsAsFactors = FALSE)
  time_coord <- data.frame(time = c("t1", "t2", "t3"),
                           stringsAsFactors = FALSE)
  data <- gridcoord::gc_expand(space_coord, time_coord)
  data$yhat <- c(1, 2, 3, 4, 5, 6)
  expect_error(validate_data_for_yhat(space_coord, time_coord, data, "my_data"),
               NA)

  # Throw an error if: some points in data aren't in time coord
  bad_data <- rbind(data, c("A", "t4", 7))
  expect_error(validate_data_for_yhat(space_coord, time_coord, bad_data, "my_data"),
               "my_data",
               class = "error_bad_data_yhat")

  # Throws an error if there are time gaps in the data:
  bad_data <- data %>%
    dplyr::filter(time != "t2")
  expect_error(validate_data_for_yhat(space_coord, time_coord, bad_data, "my_data"),
               "my_data",
               class = "error_bad_data_yhat")

  # Does *not* throw an error if there are gaps at the begining/end:
  ok_data <- data %>%
    dplyr::filter(time != "t3")
  expect_error(validate_data_for_yhat(space_coord, time_coord, ok_data, "my_data"),
               NA)
  ok_data <- data %>%
    dplyr::filter(time != "t1")
  expect_error(validate_data_for_yhat(space_coord, time_coord, ok_data, "my_data"),
               NA)
})


