# Ok, how does this work?
# Ok, what are the test?
# list of 1
# list where thing is one
# What hpapens if we want to subset over a subset of a list?
# I guess that should probably be a warning
# Ok, things to check
# 1. We always get a list out
# 2. If we put an exploded list in, we get an exploded list out
# 3. The length of the lists match up (as do the names)
# 4. Argument dispatch works correctly - including lists, data.frames, and NULL's
# 5. Works for lists of length 1

# Technically for this we don't need a space coord or a time coord...but I might do that anyway.
set.seed(338433868)
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

simple_lm_func <- function(space_coord, time_coord, data_for_model) {
  fit <- lm(y ~ time,
            data = data_for_model)
  return(list(fit = fit,
              data = data_for_model))
}

simple_lm_output <- loop_model(space_coord, time_coord, data_for_model, "y",
                               path_to_model = simple_lm_func,
                               min_train = 7,
                               use_cache = FALSE,
                               verbose = FALSE)
simple_lm_output_exploded <- loop_model(space_coord, time_coord, data_for_model, "y",
                                        path_to_model = simple_lm_func,
                                        model_arity = "uni",
                                        min_train = 7,
                                        use_cache = FALSE,
                                        verbose = FALSE)

consistent_f <- function(x, y) {
  x$coefficients[[1]] * y$y
}
test_that("loop_over behaves consistently", {
  all_over <- loop_over(space_coord, time_coord,
                        simple_lm_output[[1]],
                        consistent_f,
                        verbose = FALSE,
                        extra_args = list(y = list(y = 4)))
  expect_known_value(all_over, file = "simple_over.RDS")

  all_over <- loop_over(space_coord, time_coord,
                        simple_lm_output[[1]],
                        consistent_f,
                        verbose = FALSE,
                        extra_args = list(simple_lm_output[[2]]))
  expect_known_value(all_over, file = "simple_over_dispatch.RDS")

  all_over <- loop_over(space_coord, time_coord,
                        simple_lm_output_exploded[[1]],
                        consistent_f,
                        verbose = FALSE,
                        extra_args = list(y = list(y = 4)))
  expect_known_value(all_over, file = "exploded_over.RDS")

  all_over <- loop_over(space_coord, time_coord,
                        simple_lm_output_exploded[[1]],
                        consistent_f,
                        verbose = FALSE,
                        extra_args = list(simple_lm_output_exploded[[2]]))
  expect_known_value(all_over, file = "exploded_over_dispatch.RDS")
})



simple_time <- data.frame(time = c("a", "b", "c"),
                          stringsAsFactors = FALSE)
stub_space <- gridcoord::gc_stubcoord()
simple_list <- list("a" = 1, "b" = 2, "c" = 3)

test_that("loop_over works on simple lists", {
  res <- loop_over(stub_space, simple_time, simple_list, function(x) x*2, verbose = FALSE)
  expect_true("list" %in% class(res))
  expect_equal(length(res), length(simple_list))
  expect_equal(unname(unlist(res)), c(2, 4, 6))
})

test_that("loop_over works on lists of length 1", {
  list_of_stuff <- list("a" = c(1, 2, 3))
  res <- loop_over(stub_space, simple_time[1, , drop = FALSE], list_of_stuff, function(x) x*2, verbose = FALSE)
  expect_true("list" %in% class(res))
  expect_equal(length(res), length(list_of_stuff))
  expect_equal(res[[1]], c(2, 4, 6))
})

test_that("We can pass in data.frames or tibbles to loop_over", {
  list_of_stuff <- list("a" = 1, "b" = 2)
  df_arg <- data.frame(V1 = c(1, 2, 3), V2 = c(10, 11, 12))
  res <- loop_over(stub_space, simple_time[1:2, , drop = FALSE],
                   list_of_stuff, f = function(x, y) {x * y}, extra_args = list(df_arg))
  expect_true("list" %in% class(res))
  expect_equal(length(res), length(list_of_stuff))
  expect_equal(res[[1]], df_arg)

  tbl_arg <- tibble::as_tibble(df_arg)
  res <- loop_over(stub_space, simple_time[1:2, , drop = FALSE],
                   list_of_stuff, f = function(x, y) {x * y}, extra_args = list(tbl_arg))
  expect_true("list" %in% class(res))
  expect_equal(length(res), length(list_of_stuff))
  expect_equal(res[[1]], df_arg)
})

test_that("We can pass NULL as an extra arg", {
  test_f <- function(x, a, b) {
    if (is.null(a)) {
      return(x * b)
    } else {
      return(x * a)
    }
  }
  res <- loop_over(stub_space, simple_time,
                   simple_list, test_f, verbose = FALSE, extra_args = list(a = NULL, b = 4))
  expect_true("list" %in% class(res))
  expect_equal(length(res), length(simple_list))
  expect_equal(unname(unlist(res)), c(4, 8, 12))

  res <- loop_over(stub_space, simple_time,
                   simple_list, test_f, verbose = FALSE, extra_args = list(a = 5))
  expect_true("list" %in% class(res))
  expect_equal(length(res), length(simple_list))
  expect_equal(unname(unlist(res)), c(5, 10, 15))
})

test_that("We can pass length-zero vectors as extra args", {
  test_f <- function(x, a) {
    return(x * length(a))
  }
  res <- loop_over(stub_space, simple_time,
                   simple_list, test_f, verbose = FALSE, extra_args = list(list()))
  expect_equal(res, list("a" = 0, "b" = 0, "c" = 0))

  res <- loop_over(stub_space, simple_time,
                   simple_list, test_f, verbose = FALSE, extra_args = list(logical(0)))
  expect_equal(res, list("a" = 0, "b" = 0, "c" = 0))

  res <- loop_over(stub_space, simple_time,
                   simple_list, test_f, verbose = FALSE, extra_args = list(vector("list", 14)))
  expect_equal(res, list("a" = 14, "b" = 28, "c" = 42))
})

test_that("Components of list_of_stuff can be NULL", {
  long_time <- data.frame(time = c("a", "b", "c", "d"),
                          stringsAsFactors = FALSE)
  null_list <- list("a" = NULL, "b" = logical(0), "c" = vector("list", 5),"d" = list(NA))
  res <- loop_over(stub_space, long_time,
                   null_list, length, verbose = FALSE)
  expect_equal(res, list(a = 0, b= 0, c = 5, d = 1))
})

test_that("We dispatch if names match", {
  matched_list <- list("a" = c('a', 'a'), "b" = c('b', 'b', 'b'), "c" = c('c'))
  res <- loop_over(stub_space, simple_time,
                   simple_list, function(x, y) (x * length(y)),
                   verbose = FALSE,
                   extra_args = list(matched_list))
  expect_equal(res, list(a = 2, b = 6, c = 3))
})

test_that("verbose = FALSE means no output to the console", {
  expect_output(loop_over(stub_space, simple_time,
                          simple_list, function(x) x*2, verbose = FALSE),
                NA)
  expect_message(loop_over(stub_space, simple_time,
                           simple_list, function(x) x*2, verbose = FALSE),
                 NA)
})

# And the whole thing works equally well for gridlists.
list_s1 <- list("a" = 1, "b" = 2, "c" = 3)
list_s2 <- list("a" = 10, "b" = 20, "c" = 30)
ex_list <- list("space1" = list_s1, "space2" = list_s2)
g_list <- gridcoord::gcl_gridlist(ex_list, space_coord, simple_time, trim = )

test_that("loop_over works on exploded lists", {
  res <- loop_over(space_coord, simple_time,
                   g_list, function(x) x*2, verbose = FALSE)
  expect_true("gridlist" %in% class(res))
  expect_equal(dim(res), dim(g_list))
  expect_equal(unname(unlist(res)), c(2, 20, NA, 4, 40, NA, 6, 60, NA))
})

test_that("loop_over works on exploded lists of length 1", {
  g_list_sm <- g_list[1, ]
  res <- loop_over(space_coord[1, , drop = FALSE], simple_time,
                   g_list_sm, function(x) x*2, verbose = FALSE)
  expect_true("gridlist" %in% class(res))
  expect_equal(dim(res), dim(g_list_sm))
  expect_equal(unname(unlist(res)), c(2, 4, 6))
})

test_that("We can pass in data.frames or tibbles to exploded loop_over", {
  df_arg <- data.frame(V1 = c(1, 2, 3), V2 = c(10, 11, 12))
  res <- loop_over(space_coord, simple_time,
                   g_list, f = function(x, y) {x * y}, extra_args = list(df_arg))
  expect_true("gridlist" %in% class(res))
  expect_equal(dim(res), dim(g_list))
  for (t_list in res) {
    expect_true("list" %in% class(t_list))
    expect_equal(length(t_list), length(g_list[[1]]))
  }
  expect_equal(res[[1, 1]], df_arg)

  tbl_arg <- tibble::as_tibble(df_arg)
  res <- loop_over(space_coord, simple_time,
                   g_list, f = function(x, y) {x * y}, extra_args = list(tbl_arg))
  expect_true("gridlist" %in% class(res))
  expect_equal(dim(res), dim(g_list))
  for (t_list in res) {
    expect_true("list" %in% class(t_list))
    expect_equal(length(t_list), length(g_list[[1]]))
  }
  expect_equal(res[[1, 1]], df_arg)
})

test_that("We can pass NULL as an extra arg", {
  test_f <- function(x, a, b) {
    if (is.null(a)) {
      return(x * b)
    } else {
      return(x * a)
    }
  }
  res <- loop_over(space_coord, simple_time,
                   g_list, test_f, verbose = FALSE, extra_args = list(a = NULL, b = 4))
  expect_true("gridlist" %in% class(res))
  expect_equal(dim(res), dim(g_list))
  for (t_list in res) {
    expect_true("list" %in% class(t_list))
    expect_equal(length(t_list), length(g_list[[1]]))
  }
  expect_equal(unname(unlist(res)), c(4, 40, NA, 8, 80, NA, 12, 120, NA))

  res <- loop_over(space_coord, simple_time,
                   g_list, test_f, verbose = FALSE, extra_args = list(a = 5))
  expect_true("gridlist" %in% class(res))
  expect_equal(dim(res), dim(g_list))
  for (t_list in res) {
    expect_true("list" %in% class(t_list))
    expect_equal(length(t_list), length(g_list[[1]]))
  }
  expect_equal(unname(unlist(res)), c(5, 50, NA, 10, 100, NA, 15, 150, NA))
})

test_that("We can pass length-zero vectors as extra args", {
  test_f <- function(x, a) {
    return(x * length(a))
  }
  expected <- list(space1 = list("a" = 0, "b" = 0, "c" = 0),
                   space2 = list("a" = 0, "b" = 0, "c" = 0))
  expected <- gridcoord::gcl_gridlist(expected, space_coord, simple_time)
  res <- loop_over(space_coord, simple_time,
                   g_list, test_f, verbose = FALSE, extra_args = list(list()))
  expect_equivalent(res, expected)

  res <- loop_over(space_coord, simple_time,
                   g_list, test_f, verbose = FALSE, extra_args = list(logical(0)))
  expect_equivalent(res, expected)

  res <- loop_over(space_coord, simple_time,
                   g_list, test_f, verbose = FALSE, extra_args = list(vector("list", 14)))
  expected <- list(space1 = list("a" = 14, "b" = 28, "c" = 42),
                   space2 = list("a" = 140, "b" = 280, "c" = 420)) %>%
    gridcoord::gcl_gridlist(space_coord, simple_time)
  expect_equivalent(res, expected)
})


test_that("We dispatch if names match", {
  matched_list <- list("a" = c('a', 'a'), "b" = c('b', 'b', 'b'), "c" = c('c'))
  res <- loop_over(space_coord, simple_time,
                   g_list, function(x, y) (x * length(y)),
                   verbose = FALSE,
                   extra_args = list(matched_list))
  expected <- list(space1 = list("a" = 2, "b" = 6, "c" = 3),
                   space2 = list("a" = 20, "b" = 60, "c" = 30)) %>%
    gridcoord::gcl_gridlist(space_coord, simple_time)
  expect_equivalent(res, expected)
})

test_that("verbose = FALSE means no output to the console", {
  expect_output(loop_over(space_coord, simple_time,
                          g_list, function(x) x*2, verbose = FALSE),
                NA)
  expect_message(loop_over(space_coord, simple_time,
                           g_list, function(x) x*2, verbose = FALSE),
                 NA)
})
