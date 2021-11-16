space_coord <- data.frame(space_label = c("space1", "space2", "space3"),
                          stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()

start_times <- 4:8
fin_times <- start_times + 1L
labels <- paste0("X", start_times) #sapply(start_times + 64, intToUtf8)
time_coord <- data.frame(time_labels = labels,
                         time = start_times,
                         fin_time = fin_times,
                         stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()


reference_l <- as.list(start_times + 27)
names(reference_l) <- gridcoord::gc_get_labels(time_coord)

reference_gl <- rep(list(reference_l), 3)
names(reference_gl) <- gridcoord::gc_get_labels(space_coord)
reference_gl <- reference_gl %>%
  gridcoord::gcl_gridlist(space_coord, time_coord)

reference_l[1:3] <- NA
reference_gl[1:2, 1:3] <- NA

test_that("single arguments to lists", {
  arg_list <- list(a = 3)
  res <- pad_args(arg_list, reference_l)
  expect_equal(names(res), names(reference_l))
  lapply(res, expect_equal, list(a = 3))
})

test_that("Multiple simple arguments to lists", {
  arg_list <- list(a = 3, b = c("a", "b", "c"))
  res <- pad_args(arg_list, reference_l)
  expect_equal(names(res), names(reference_l))
  lapply(res, expect_equal, list(a = 3, b = c("a", "b", "c")))
})

test_that("Single simple argument to gridlist", {
  arg_list <- list(a = TRUE)
  res <- pad_args(arg_list, reference_gl)
  expect_equal(rownames(res), rownames(reference_gl))
  expect_equal(colnames(res), colnames(reference_gl))
  expect_equal(res[[3, 4]], list(a = TRUE))
})

test_that("Multiple simple argument to gridlist", {
  arg_list <- list(a = TRUE, b = c(NA, "b", "c"))
  res <- pad_args(arg_list, reference_gl)
  expect_equal(rownames(res), rownames(reference_gl))
  expect_equal(colnames(res), colnames(reference_gl))
  expect_equal(res[[3, 4]], list(a = TRUE, b = c(NA, "b", "c")))
})

test_that("Multiple unnamed simple arguments to gridlist", {
  arg_list <- list(TRUE, c(NA, "b", "c"))
  res <- pad_args(arg_list, reference_gl)
  expect_equal(rownames(res), rownames(reference_gl))
  expect_equal(colnames(res), colnames(reference_gl))
  expect_equal(res[[3, 4]], list(TRUE, c(NA, "b", "c")))
})

time_dispatched <- list(X4 = "a", X5 = "b", X6 = "c",
                            X7 = c("d", "e"), X8 = c("f", "g", "h"))
space_dispatched <- list(space1 = 1, space2 = 2, space3 = c(NA, NA))

st_dispatched <- list(space1 = time_dispatched,
                      space2 = lapply(time_dispatched, toupper),
                      space3 = lapply(time_dispatched, sub, pattern = "d", replacement = "z"))
test_that("Single dispatched argument to list", {
  arg_list <- list(a = time_dispatched)
  res <- pad_args(arg_list, reference_l)
  expect_equal(names(res), names(reference_l))
  expect_equal(res$X6$a, time_dispatched$X6)
})

test_that("Single dispatched argument to gridlist", {
  arg_list <- list(a = time_dispatched)
  res <- pad_args(arg_list, reference_gl)
  expect_equal(colnames(res), colnames(reference_gl))
  expect_equal(rownames(res), rownames(reference_gl))
  expect_equal(res[[3, 4]]$a, time_dispatched[[4]])
  expect_equal(res[[1, 2]]$a, time_dispatched[[2]])

  arg_list <- list(b = space_dispatched)
  res <- pad_args(arg_list, reference_gl)
  expect_equal(colnames(res), colnames(reference_gl))
  expect_equal(rownames(res), rownames(reference_gl))
  expect_equal(res[[3, 4]]$b, space_dispatched[[3]])
  expect_equal(res[[1, 2]]$b, space_dispatched[[1]])

  arg_list <- list(c = st_dispatched)
  res <- pad_args(arg_list, reference_gl)
  expect_equal(colnames(res), colnames(reference_gl))
  expect_equal(rownames(res), rownames(reference_gl))
  expect_equal(res[[3, 4]]$c, st_dispatched[[3]][[4]])
  expect_equal(res[[1, 2]]$c, st_dispatched[[1]][[2]])
})

# Ok, those are the super-basics I think
test_that("Multiple forms of dispatched to gridlist", {
  arg_list <- list(a = time_dispatched,
                   b = space_dispatched,
                   c = st_dispatched,
                   d = "bananas")
  res <- pad_args(arg_list, reference_gl)
  expect_equal(colnames(res), colnames(reference_gl))
  expect_equal(rownames(res), rownames(reference_gl))

  expect_equal(res[[3, 4]], list(a = time_dispatched[[4]],
                                 b = space_dispatched[[3]],
                                 c = st_dispatched[[3]][[4]],
                                 d = "bananas"))
  expect_equal(res[[1, 2]], list(a = time_dispatched[[2]],
                                 b = space_dispatched[[1]],
                                 c = st_dispatched[[1]][[2]],
                                 d = "bananas"))
})

df <- data.frame(x = c(1, 2, 3), y = c(2, 4, 5))
l <- as.list(df)
fit <- lm(y ~ x, data = df)

test_that("Padding works on lists, data.frames, and other confusing objects", {
  arg_list <- list(df = df,
                   l = l,
                   fit = fit,
                   a = time_dispatched,
                   b = space_dispatched)
  res <- pad_args(arg_list, reference_l)
  expect_equal(names(res), names(reference_l))
  expect_equal(res[[4]], list(df = df,
                              l = l,
                              fit = fit,
                              a = time_dispatched[[4]],
                              b = space_dispatched))

  res <- pad_args(arg_list, reference_gl)
  expect_equal(colnames(res), colnames(reference_gl))
  expect_equal(rownames(res), rownames(reference_gl))
  expect_equal(res[[3, 4]], list(df = df,
                              l = l,
                              fit = fit,
                              a = time_dispatched[[4]],
                              b = space_dispatched[[3]]))
})

test_that("Still dispatch is we don't have the NA entries", {
  trimmed_td <- time_dispatched[3:5]
  arg_list <- list(a = trimmed_td)

  res <- pad_args(arg_list, reference_l)
  expect_equal(names(res), names(reference_l))
  expect_equal(res[[1]], list(a = NA))
  expect_equal(res[[3]], list(a = trimmed_td[[1]]))
  expect_equal(res[[5]], list(a = trimmed_td[[3]]))
})

test_that("Don't dispatch is we don't have any of the other entries", {
  trimmed_td <- time_dispatched[1:4]
  arg_list <- list(a = trimmed_td)
  res <- pad_args(arg_list, reference_l)
  expect_equal(names(res), names(reference_l))
  expect_equal(res[[1]], list(a = trimmed_td))

  res <- pad_args(arg_list, reference_gl)
  expect_equal(colnames(res), colnames(reference_gl))
  expect_equal(rownames(res), rownames(reference_gl))
  expect_equal(res[[3, 4]], list(a = trimmed_td))
})

test_that("Don't dispatch is we have extra entries", {
  extra_td <- time_dispatched
  extra_td["X13"] <- 109
  arg_list <- list(a = extra_td)
  res <- pad_args(arg_list, reference_l)
  expect_equal(names(res), names(reference_l))
  expect_equal(res[[1]], list(a = extra_td))

  res <- pad_args(arg_list, reference_gl)
  expect_equal(colnames(res), colnames(reference_gl))
  expect_equal(rownames(res), rownames(reference_gl))
  expect_equal(res[[3, 4]], list(a = extra_td))
})



test_that("Padding works on lists, data.frames, and other confusing objects", {
  arg_list <- list(df = df,
                   l = l,
                   fit = fit,
                   a = time_dispatched,
                   b = space_dispatched,
                   n = NULL)
  res <- pad_args(arg_list, reference_l)
  expect_equal(names(res), names(reference_l))
  expect_equal(res[[4]], list(df = df,
                              l = l,
                              fit = fit,
                              a = time_dispatched[[4]],
                              b = space_dispatched,
                              n = NULL))

  res <- pad_args(arg_list, reference_gl)
  expect_equal(colnames(res), colnames(reference_gl))
  expect_equal(rownames(res), rownames(reference_gl))
  expect_equal(res[[3, 4]], list(df = df,
                                 l = l,
                                 fit = fit,
                                 a = time_dispatched[[4]],
                                 b = space_dispatched[[3]],
                                 n = NULL))
})


# a1 <- list(a = list(X4 = 1, X5 = 17), b = list(X4 = NULL, X5 = NULL))
# list_transpose(a1)

