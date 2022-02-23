set.seed(65616181)
curr_options <- options(statsurv.progress = FALSE)
on.exit(options(curr_options), add = TRUE)
# Key rule for rowmute is that it should match dplyr done rowwise.


starting_df <- data.frame(group = rep(1:10, each = 10),
                          x = rnorm(100))
starting_df$y <- starting_df$x * 2.04 + 1.23
nested_df <- starting_df %>%
  tidyr::nest(curr_data = -group) %>%
  dplyr::mutate(even = group %% 2) %>%
  dplyr::ungroup()

test_that("rowmute matches dplyr", {
  v1 <- nested_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(fit = list(lm(y ~ x, data = curr_data))) %>%
    dplyr::ungroup()
  v2 <- nested_df %>%
    rowmute(fit = lm(y ~ x, data = curr_data))
  expect_equal(v1, v2)
})

test_that("auto-unlisting works on scalars", {
  v1 <- nested_df %>%
    dplyr::mutate(q = group + 15)
  v2 <- nested_df %>%
    rowmute(q = group + 15)
  expect_equal(v1, v2)
  expect_true(rlang::is_atomic(v2$q))
})

test_that("auto-unlisting doesn't work on non-scalar atomics", {
  v1 <- nested_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(q = list(c(group + 15, group - 1))) %>%
    dplyr::ungroup()

  v2 <- nested_df %>%
    rowmute(q = list(c(group + 15, group - 1)))
  expect_equal(v1, v2)
  expect_true(rlang::is_list(v2$q))
})

test_that("auto-unlisting behaves correctly for dates", {
  nested_df$date_chr = "2022-01-22"
  v1 <- nested_df %>%
    dplyr::mutate(date_date = lubridate::ymd(date_chr))
  v2 <- nested_df %>%
    rowmute(date_date = lubridate::ymd(date_chr))
  expect_equal(v1, v2)
})


test_that("rowmute works on factors", {
  v1 <- nested_df %>%
    dplyr::mutate(group_f = factor(group))
  v2 <- nested_df %>%
    rowmute(group_f = factor(group))
  expect_equal(v1, v2)
  expect_true(is.factor(v2$group_f))
})

test_that("rowmute works on functions", {
  v1 <- nested_df %>%
    dplyr::mutate(func = list(mean))
  v2 <- nested_df %>%
    rowmute(func = mean)
  expect_equal(v1, v2)
})

test_that("rowmute works with user-defined functions", {
  # I get an error if I run this through the build panel, but not if I run it at the command line.
  # I think that's a tricky error involving environments, so it'll take some thought to debug.
  f <- function(x, y) {
    x + 2*y
  }
  # Needed to make the environment finding work properly. I don't like it though.
  f <<- f
  v1 <- nested_df %>%
    dplyr::mutate(z = f(group, even))
  v2 <- nested_df %>%
    rowmute(z = f(group, even))
  expect_equal(v1, v2)
})

test_that("rowmute works on record-style objects", {
  new_rational <- function(n = integer(), d = integer()) {
    vctrs::vec_assert(n, ptype = integer())
    vctrs::vec_assert(d, ptype = integer())

    vctrs::new_rcrd(list(n = n, d = d), class = "vctrs_rational")
  }

  new_rational <<- new_rational
  format.vctrs_rational <- function(x, ...) {
    n <- vctrs::field(x, "n")
    d <- vctrs::field(x, "d")

    out <- paste0(n, "/", d)
    out[is.na(n) | is.na(d)] <- NA

    out
  }

  df <- tibble::tibble(n = c(1L, 2L, 3L), d = 2L)
  v1 <- df %>%
    dplyr::mutate(frac = new_rational(n, d)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(frac_l = list(c(new_rational(n, d), new_rational(n, 3L)))) %>%
    dplyr::ungroup()

  v2 <- df %>%
    statsurv::rowmute(frac = new_rational(n, d),
            frac_l = list(c(new_rational(n, d), new_rational(n, 3L)))) %>%
    tibble::as_tibble()
  expect_equal(v1, v2)
  expect_true("vctrs_rational" %in% class(v2$frac) )
  expect_true(class(v2$frac_l) == "list")
})

