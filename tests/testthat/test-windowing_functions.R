set.seed(54515529)

x = rnorm(100)
spacetime_data <- data.frame(id_space = rep(1:10, each = 10),
                             id_time = rep(1:10, 10),
                             x = x,
                             y = 2.04 * x + 1.23)
spacetime_data$f <- rep(c(min, max, median, mean), each = 25)
spacetime_data$df <- data.frame(xx = 1:100, yy = 201:300)
organize_df <- function(df) {
  df %>%
    dplyr::arrange(dplyr::across()) %>%
    as.data.frame() %>%
    tibble::remove_rownames()
}

test_that("Fail appropriately on badly formed inputs", {
  bad_df <- dplyr::select(spacetime_data, -id_time)
  expect_error(window_idtime(bad_df, min_train = 1, max_train = 1, n_predict = 1))
  expect_error(window_idtime(spacetime_data, min_train = 0.5))
  expect_error(window_idtime(spacetime_data, min_train = "canteloupe"))
  expect_error(window_idtime(spacetime_data, min_train = c(2, 3)))
  expect_error(window_idtime(spacetime_data, min_train = 1, max_train = 2.5))
  expect_error(window_idtime(spacetime_data, min_train = 1, max_train = "honeydew"))
  expect_error(window_idtime(spacetime_data, min_train = 1, max_train = c(3, 4)))
  expect_error(window_idtime(spacetime_data, min_train = 1, n_predict = pi))
  expect_error(window_idtime(spacetime_data, min_train = 1, n_predict = "korean"))
  expect_error(window_idtime(spacetime_data, min_train = 1, n_predict = c(4, 5)))

  expect_error(window_idtime(spacetime_data, min_train = 1, split_spatial_locations = "hello"))
  expect_error(window_idtime(spacetime_data, min_train = 1, split_spatial_locations = 2))
  expect_error(window_idtime(spacetime_data, min_train = 1, split_spatial_locations = c("uni", "uni")))

  n <- nrow(spacetime_data)
  expect_error(window_idtime(spacetime_data, min_train = 0))
  expect_error(window_idtime(spacetime_data, min_train = n + 1))
  expect_error(window_idtime(spacetime_data, min_train = 1, n_predict = -1))
  expect_error(window_idtime(spacetime_data, min_train = 1, n_predict = n + 1))
  expect_error(window_idtime(spacetime_data, min_train = 4, max_train = 3))
  expect_error(window_idtime(spacetime_data, min_train = 4, n_predict = n - 4 + 1))

  expect_error(window_idtime(spacetime_data, min_train = 1, step = 1.5))
  expect_error(window_idtime(spacetime_data, min_train = 1, step = -2))
  expect_error(window_idtime(spacetime_data, min_train = 1, step = c(2, 3)))
  expect_error(window_idtime(spacetime_data, min_train = 1, step = "Hello"))

})

test_that("When we window, filter out the data that's not complete", {
  for (ii in 1:10) {
    n <- sample(1:5, size = 1)
    mt <- sample(1:(10 - n), size = 1)

    windowed_df <- window_idtime(spacetime_data, min_train = mt, max_train = mt,
                                 n_predict = n, split_spatial_locations = FALSE)
    expect_equal(min(windowed_df$window_time_id ), mt + n)
    for (jj in seq_len(nrow(windowed_df))) {
      expected_df <- spacetime_data %>%
        dplyr::filter(id_time >= windowed_df$window_time_id [[jj]] - mt - n + 1,
                      id_time <= windowed_df$window_time_id [[jj]]) %>%
        organize_df()
      windowed_data <- windowed_df$curr_data[[jj]] %>%
        organize_df()
      expect_equal(windowed_data,
                   expected_df)
    }
  }
})

test_that("Works when max_train != min_train", {
  for (ii in 1:10) {
    n <- sample(1:5, size = 1)
    mt <- sample(1:(9 - n), size = 1)
    maxt <- sample(mt:(10 - n), size = 1)

    windowed_df <- window_idtime(spacetime_data, min_train = mt, max_train = maxt,
                                 n_predict = n, split_spatial_locations = FALSE)
    expect_equal(min(windowed_df$window_time_id ), mt + n)
    for (jj in seq_len(nrow(windowed_df))) {
      start <- max(windowed_df$window_time_id [[jj]] - maxt - n + 1, 1)
      expected_df <- spacetime_data %>%
        dplyr::filter(id_time >= start,
                      id_time <= windowed_df$window_time_id [[jj]]) %>%
        organize_df()
      windowed_data <- windowed_df$curr_data[[jj]] %>%
        organize_df()
      expect_equal(windowed_data,
                   expected_df)
    }
  }
})




# Need to check behavior for split_spatial_locations = TRUE
test_that("Windowing works for min_train = max_train, split_spatial_locations = TRUE", {
  for (ii in 1:10) {
    n <- sample(1:5, size = 1)
    mt <- sample(1:(10 - n), size = 1)

    windowed_df <- window_idtime(spacetime_data, min_train = mt, max_train = mt,
                                 n_predict = n, split_spatial_locations = TRUE)
    expect_equal(min(windowed_df$window_time_id ), mt + n)
    for (jj in seq_len(nrow(windowed_df))) {
      expected_df <- spacetime_data %>%
        dplyr::filter(id_time >= windowed_df$window_time_id [[jj]] - mt - n + 1,
                      id_time <= windowed_df$window_time_id [[jj]],
                      id_space == windowed_df$window_space_id[[jj]]) %>%
        organize_df()
      windowed_data <- windowed_df$curr_data[[jj]] %>%
        organize_df()
      expect_equal(windowed_data,
                   expected_df)
    }
  }
})

test_that("Works when max_train != min_train and split_spatial_locations = TRUE", {
  for (ii in 1:10) {
    n <- sample(1:5, size = 1)
    mt <- sample(1:(9 - n), size = 1)
    maxt <- sample(mt:(10 - n), size = 1)

    windowed_df <- window_idtime(spacetime_data, min_train = mt, max_train = maxt,
                                 n_predict = n, split_spatial_locations = TRUE)
    expect_equal(min(windowed_df$window_time_id), mt + n)
    for (jj in seq_len(nrow(windowed_df))) {
      start <- max(windowed_df$window_time_id[[jj]] - maxt - n + 1, 1)
      expected_df <- spacetime_data %>%
        dplyr::filter(id_time >= start,
                      id_time <= windowed_df$window_time_id[[jj]],
                      id_space == windowed_df$window_space_id[[jj]]) %>%
        organize_df()
      windowed_data <- windowed_df$curr_data[[jj]] %>%
        organize_df()
      expect_equal(windowed_data,
                   expected_df)
    }
  }
})


test_that("Works when max_train != min_train and split_spatial_locations = TRUE", {
  for (ii in 1:10) {
    n <- sample(1:5, size = 1)
    mt <- sample(1:(9 - n), size = 1)
    maxt <- sample(mt:(10 - n), size = 1)

    windowed_df <- window_idtime(spacetime_data, min_train = mt, max_train = maxt,
                                 n_predict = n, split_spatial_locations = TRUE)
    expect_equal(min(windowed_df$window_time_id), mt + n)
    for (jj in seq_len(nrow(windowed_df))) {
      start <- max(windowed_df$window_time_id[[jj]] - maxt - n + 1, 1)
      expected_df <- spacetime_data %>%
        dplyr::filter(id_time >= start,
                      id_time <= windowed_df$window_time_id[[jj]],
                      id_space == windowed_df$window_space_id[[jj]]) %>%
        organize_df()
      windowed_data <- windowed_df$curr_data[[jj]] %>%
        organize_df()
      expect_equal(windowed_data,
                   expected_df)
    }
  }
})

test_that("Works when step != 1", {
  for (ii in 1:30) {
    n <- sample(1:5, size = 1)
    mt <- sample(1:(9 - n), size = 1)
    maxt <- sample(mt:(10 - n), size = 1)
    stp <- sample(1:5, size = 1)

    windowed_df <- window_idtime(spacetime_data, min_train = mt, max_train = maxt,
                                 n_predict = n, split_spatial_locations = TRUE,
                                 step = stp)
    expect_equal(min(windowed_df$window_time_id), mt + n)
    expect_equal(length(unique(windowed_df$window_space_id)), length(unique(spacetime_data$id_space)))
    if (mt + n + stp > max(spacetime_data$id_time)) {
      expect_equal(length(unique(windowed_df$window_time_id)), 1)
    } else {
      u_ids = sort(unique(windowed_df$window_time_id))
      expect_true(all(diff(u_ids) == stp))
    }


    for (jj in seq_len(nrow(windowed_df))) {
      start <- max(windowed_df$window_time_id[[jj]] - maxt - n + 1, 1)
      expected_df <- spacetime_data %>%
        dplyr::filter(id_time >= start,
                      id_time <= windowed_df$window_time_id[[jj]],
                      id_space == windowed_df$window_space_id[[jj]]) %>%
        organize_df()
      windowed_data <- windowed_df$curr_data[[jj]] %>%
        organize_df()
      expect_equal(windowed_data,
                   expected_df)
    }
  }

})

test_that("Works when step != 1, split_spatial_locations = FALSE", {
  for (ii in 1:30) {
    n <- sample(1:5, size = 1)
    mt <- sample(1:(9 - n), size = 1)
    maxt <- sample(mt:(10 - n), size = 1)
    stp <- sample(1:5, size = 1)

    windowed_df <- window_idtime(spacetime_data, min_train = mt, max_train = maxt,
                                 n_predict = n, split_spatial_locations = FALSE,
                                 step = stp)
    expect_equal(min(windowed_df$window_time_id), mt + n)
    expect_equal(length(unique(windowed_df$window_space_id)), 1)
    if (mt + n + stp > max(spacetime_data$id_time)) {
      expect_equal(length(unique(windowed_df$window_time_id)), 1)
    } else {
      u_ids = sort(unique(windowed_df$window_time_id))
      expect_true(all(diff(u_ids) == stp))
    }


    for (jj in seq_len(nrow(windowed_df))) {
      start <- max(windowed_df$window_time_id[[jj]] - maxt - n + 1, 1)
      expected_df <- spacetime_data %>%
        dplyr::filter(id_time >= start,
                      id_time <= windowed_df$window_time_id[[jj]]) %>%
        organize_df()
      windowed_data <- windowed_df$curr_data[[jj]] %>%
        organize_df()
      expect_equal(windowed_data,
                   expected_df)
    }
  }

})

