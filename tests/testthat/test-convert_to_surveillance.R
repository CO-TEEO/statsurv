# convert_to_surveillance takes a set of data.frames and effectively pulls out the
# last n-rows in each one.

set.seed(986561351)


x = rnorm(100)
spacetime_data <- data.frame(id_space = rep(1:10, each = 10),
                             id_time = rep(1:10, 10),
                             x = x,
                             y = 2.04 * x + 1.23)

check_surv <- function(surv_dfs, base, .id_time) {
  check_entry <- function(x, base) {
    expect_equal(dplyr::filter(x, id_time == .id_time),
                 dplyr::filter(base, id_time == .id_time))
  }
  purrr::walk(surv_dfs, check_entry, base = base)
}
check_lengths <- function(surv_dfs, lengths) {
  expect_equal(purrr::map_dbl(surv_dfs, nrow),
               lengths)
}

test_that("Works for n_predict = 1, model_arity = 'multi'", {
  base_df <- window_idtime(spacetime_data, min_train = 5, max_train = 8, n_predict = 1, model_arity = "multi") %>%
    rowmute(aug_data = dplyr::mutate(curr_data, .fitted = rnorm(dplyr::n())))

  surv_df <- base_df %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = FALSE,
                                                               grow_length = FALSE))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(10, 20, 30, 40, 50))

  surv_df <- base_df %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = TRUE,
                                                               grow_length = FALSE))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 2)
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(60, 70, 80, 90, 90))

  surv_df <- base_df %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = FALSE,
                                                               grow_length = TRUE))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(10, 20, 30, 40, 50))

  surv_df <- base_df %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = TRUE,
                                                               grow_length = TRUE))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 2)
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(60, 70, 80, 90, 100))
})

test_that("Works for n_predict = 1, model_arity = 'uni'", {
  base_df <- window_idtime(spacetime_data, min_train = 5, max_train = 8, n_predict = 1, model_arity = "uni") %>%
    rowmute(aug_data = dplyr::mutate(curr_data, .fitted = rnorm(dplyr::n())))

  big_surv_df <- base_df %>%
    dplyr::group_by(id_space) %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = FALSE,
                                                               grow_length = FALSE)) %>%
    dplyr::ungroup()
  surv_df <- big_surv_df %>%
    dplyr::filter(id_space == sample.int(10, size = 1))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(1, 2, 3, 4, 5))

  big_surv_df <- base_df %>%
    dplyr::group_by(id_space) %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = TRUE,
                                                               grow_length = FALSE)) %>%
    dplyr::ungroup()
  z <- sample.int(10, size = 1)
  surv_df <- big_surv_df %>%
    dplyr::filter(id_space == z)
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 2)
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(6, 7, 8, 9, 9))

  big_surv_df <- base_df %>%
    dplyr::group_by(id_space) %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = FALSE,
                                                               grow_length = TRUE)) %>%
    dplyr::ungroup()

  surv_df <- big_surv_df %>%
    dplyr::filter(id_space == sample.int(10, size = 1))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(1, 2, 3, 4, 5))

  big_surv_df <- base_df %>%
    dplyr::group_by(id_space) %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = TRUE,
                                                               grow_length = TRUE)) %>%
    dplyr::ungroup()
  surv_df <- big_surv_df %>%
    dplyr::filter(id_space == sample.int(10, size = 1))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 2)
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(6, 7, 8, 9, 10))
})

test_that("Gives an error if id_space doesn't line up", {
  split_df <- window_idtime(spacetime_data, min_train = 5, max_train = 8, n_predict = 1,
                           model_arity = "uni") %>%
    rowmute(aug_data = dplyr::mutate(curr_data, .fitted = rnorm(dplyr::n())))
  comb_df <- window_idtime(spacetime_data, min_train = 5, max_train = 8, n_predict = 1,
                           model_arity = "multi") %>%
    rowmute(aug_data = dplyr::mutate(curr_data, .fitted = rnorm(dplyr::n())))
  expect_error({big_surv_df <- split_df %>%
                 dplyr::group_by(id_space) %>%
                 dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                                            include_init = TRUE,
                                                                            grow_length = TRUE)) %>%
                 dplyr::ungroup()},
               NA)
  expect_error({big_surv_df <- split_df %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = TRUE,
                                                               grow_length = TRUE)) }
    )

  # We can override the error with check_space_ids = FALSE
  expect_error({split_df %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = FALSE,
                                                               grow_length = FALSE,
                                                               check_space_ids = FALSE)) },
    NA)
})

test_that("Works for n_predict = 3, model_arity = 'multi'", {
  base_df <- window_idtime(spacetime_data, min_train = 5, max_train = 6, n_predict = 3, model_arity = "multi") %>%
    rowmute(aug_data = dplyr::mutate(curr_data, .fitted = rnorm(dplyr::n())))

  surv_df <- base_df %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = FALSE,
                                                               grow_length = FALSE))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(30, 40, 50))

  surv_df <- base_df %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = TRUE,
                                                               grow_length = FALSE))

  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 2)
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(80, 90, 90))

  surv_df <- base_df %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = FALSE,
                                                               grow_length = TRUE))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(30, 40, 50))

  surv_df <- base_df %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = TRUE,
                                                               grow_length = TRUE))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 2)
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(80, 90, 100))
})

test_that("Works for n_predict = 3, model_arity = 'uni'", {
  base_df <- window_idtime(spacetime_data, min_train = 5, max_train = 6, n_predict = 3, model_arity = "uni") %>%
    rowmute(aug_data = dplyr::mutate(curr_data, .fitted = rnorm(dplyr::n())))

  big_surv_df <- base_df %>%
    dplyr::group_by(id_space) %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = FALSE,
                                                               grow_length = FALSE)) %>%
    dplyr::ungroup()
  surv_df <- big_surv_df %>%
    dplyr::filter(id_space == sample.int(10, size = 1))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(3, 4, 5))

  big_surv_df <- base_df %>%
    dplyr::group_by(id_space) %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = TRUE,
                                                               grow_length = FALSE)) %>%
    dplyr::ungroup()
  z <- sample.int(10, size = 1)
  surv_df <- big_surv_df %>%
    dplyr::filter(id_space == z)
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 2)
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(8, 9, 9))

  big_surv_df <- base_df %>%
    dplyr::group_by(id_space) %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = FALSE,
                                                               grow_length = TRUE)) %>%
    dplyr::ungroup()

  surv_df <- big_surv_df %>%
    dplyr::filter(id_space == sample.int(10, size = 1))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(3, 4, 5))

  big_surv_df <- base_df %>%
    dplyr::group_by(id_space) %>%
    dplyr::mutate(surv_data = calculate_surveillance_residuals(aug_data, split_id,
                                                               include_init = TRUE,
                                                               grow_length = TRUE)) %>%
    dplyr::ungroup()
  surv_df <- big_surv_df %>%
    dplyr::filter(id_space == sample.int(10, size = 1))
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 2)
  check_surv(surv_df$surv_data, surv_df$aug_data[[1]], .id_time = 6)
  check_surv(surv_df$surv_data[-1], surv_df$aug_data[[2]], .id_time = 7)
  check_lengths(surv_df$surv_data, c(8, 9, 10))
})
