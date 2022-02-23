listy <- function(vec) {
  unname(split(vec, seq_len(length(vec))))
}
check_vec <- function(vec) {
  if (is.list(vec)) {
    df <- tibble::tibble(x = vec)
  } else {
    df <- tibble::tibble(x = listy(vec))
  }
  expect_equal(unlist_scalars(df)$x, unlist(vec))
}

test_that("unnest scalars works on weird data types", {
  dates <- lubridate::ymd(c("2022-01-01", "2021-01-01"))
  check_vec(dates)

  other_dates <- as.POSIXlt(dates)
  df <- tibble::tibble(x = list(other_dates[[1]], other_dates[[2]]))
  expect_equal(unlist_scalars(df)$x, other_dates)

  fs <- c(mean, median, min)
  check_vec(fs)

  factors <- list(factor("A"), factor("B"), factor("C"))
  df <- tibble::tibble(x = factors)
  expect_equal(unlist_scalars(df)$x, unlist(factors))
})

test_that("unlist_scalars works on record-style objects", {
  new_rational <- function(n = integer(), d = integer()) {
    vctrs::vec_assert(n, ptype = integer())
    vctrs::vec_assert(d, ptype = integer())

    vctrs::new_rcrd(list(n = n, d = d), class = "vctrs_rational")
  }

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
    dplyr::mutate(frac_l = list(c(new_rational(n, d)))) %>%
    dplyr::ungroup()
  v2 <- unlist_scalars(v1)
  expect_equal(v2$frac_l, v1$frac)
})

test_that("unlist_scalars doesn't unlist stuff it shouldn't unlist", {
  fit <- lm(mpg ~ wt, data = mtcars)
  list_df <- tibble::tibble(fits = list(fit, fit, fit),
                 funcs = list(mean, median, mode),
                 list_of_lists = list(list(1), list(2), list(3, 2)))
  # What should happen with a list of lists? My instinct is that they should stay as is.
  expect_equal(unlist_scalars(list_df), list_df)
})

test_that("collapse turns things into lists", {
  df <- tibble::tibble(id = c(1, 2, 2),
                       factors = factor(c("A", "B", "C")),
                       strs = c("A", "B", "B"),
                       funcs = list(mean, median, median),
                       lists = list(list(1), list(2), list(1)),
                       dfs = list(data.frame(x = 1), data.frame(x = 2), data.frame(x = 2)))

  collapsed_df <- df %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = collapse, combine_dfs = FALSE,
                                   unique_only = FALSE))
  expect_equal(nrow(collapsed_df), 1)
  expect_equal(collapsed_df$id, list(c(1, 2, 2)))
  expect_equal(collapsed_df$factors, list(factor(c("A", "B", "C"))))
  expect_equal(collapsed_df$strs, list(c("A", "B", "B")))
  expect_equal(collapsed_df$funcs, list(c(mean, median, median)))
  expect_equal(collapsed_df$dfs, list(list(data.frame(x = 1), data.frame(x = 2), data.frame(x = 2))))
  # This is where it gets confusing to even reason about what this should be.
  # But this is actually now a three-level deep nested list
  expect_equal(collapsed_df$lists, list(list(list(1), list(2), list(1))))
  collapsed_df2 <- df %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = collapse, combine_dfs = TRUE,
                                   unique_only = FALSE))
  expect_equal(dplyr::select(collapsed_df, -dfs), dplyr::select(collapsed_df2, -dfs))
  expect_equal(collapsed_df2$dfs, list(data.frame(x = c(1, 2, 2))))

  expect_equal(collapsed_df, collapse_all(df, combine_dfs = FALSE, unique_only = FALSE, unlist_scalars = FALSE))
  expect_equal(collapsed_df2, collapse_all(df, combine_dfs = TRUE, unique_only = FALSE, unlist_scalars = FALSE))

  # Then repeat with unique_only = TRUE
  collapsed_df <- df %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = collapse, combine_dfs = FALSE,
                                   unique_only = TRUE))
  expect_equal(nrow(collapsed_df), 1)
  expect_equal(collapsed_df$id, list(c(1, 2)))
  expect_equal(collapsed_df$factors, list(factor(c("A", "B", "C"))))
  expect_equal(collapsed_df$strs, list(c("A", "B")))
  expect_equal(collapsed_df$funcs, list(c(mean, median)))
  expect_equal(collapsed_df$dfs, list(list(data.frame(x = 1), data.frame(x = 2))))
  expect_equal(collapsed_df$lists, list(list(list(1), list(2))))
  collapsed_df2 <- df %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = collapse, combine_dfs = TRUE,
                                   unique_only = TRUE))
  expect_equal(dplyr::select(collapsed_df, -dfs), dplyr::select(collapsed_df2, -dfs))
  expect_equal(collapsed_df2$dfs, list(data.frame(x = c(1, 2, 2))))
  expect_equal(collapsed_df, collapse_all(df, combine_dfs = FALSE, unique_only = TRUE, unlist_scalars = FALSE))
  expect_equal(collapsed_df2, collapse_all(df, combine_dfs = TRUE, unique_only = TRUE, unlist_scalars = FALSE))

  # Then some quick checks to make sure our groupings work
  collapsed_df <- df %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = collapse, combine_dfs = FALSE,
                                   unique_only = FALSE))
  expect_equal(nrow(collapsed_df), 2)
  expect_equal(collapsed_df$id, c(1, 2))
  expect_equal(collapsed_df$funcs, list(mean, list(median, median)))
  expect_equal(collapsed_df$dfs, list(data.frame(x = 1), list(data.frame(x = 2), data.frame(x = 2))))
  collapsed_df2 <- df %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = collapse, combine_dfs = TRUE,
                                   unique_only = FALSE))
  expect_equal(dplyr::select(collapsed_df, -dfs), dplyr::select(collapsed_df2, -dfs))
  expect_equal(collapsed_df2$dfs, list(data.frame(x = 1), data.frame(x = c(2, 2))))
  expect_equal(collapsed_df, collapse_all(dplyr::group_by(df, id),
                                          combine_dfs = FALSE, unique_only = FALSE, unlist_scalars = FALSE))
  expect_equal(collapsed_df2, collapse_all(dplyr::group_by(df, id),
                                           combine_dfs = TRUE, unique_only = FALSE, unlist_scalars = FALSE))


  # And with unique_only = TRUE
  collapsed_df <- df %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = collapse, combine_dfs = FALSE,
                                   unique_only = TRUE))
  expect_equal(nrow(collapsed_df), 2)
  expect_equal(collapsed_df$id, c(1, 2))
  expect_equal(collapsed_df$funcs, list(mean, median))
  expect_equal(collapsed_df$dfs, list(data.frame(x = 1), data.frame(x = 2)))
  collapsed_df2 <- df %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = collapse, combine_dfs = TRUE,
                                   unique_only = TRUE))
  expect_equal(dplyr::select(collapsed_df, -dfs), dplyr::select(collapsed_df2, -dfs))
  expect_equal(collapsed_df2$dfs, list(data.frame(x = 1), data.frame(x = c(2, 2))))
  expect_equal(collapsed_df, collapse_all(dplyr::group_by(df, id),
                                          combine_dfs = FALSE, unique_only = TRUE, unlist_scalars = FALSE))
  expect_equal(collapsed_df2, collapse_all(dplyr::group_by(df, id),
                                           combine_dfs = TRUE, unique_only = TRUE, unlist_scalars = FALSE))

})
