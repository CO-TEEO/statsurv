space_coord <- data.frame(space = c("A", "B"),
                          stringsAsFactors = FALSE)
time_coord <- data.frame(time = c("t1", "t2", "t3"),
                         stringsAsFactors = FALSE)
data <- gridcoord::gc_expand(space_coord, time_coord)
data$data_col <- 1:6
data$word_col <- c("a", "b", "c", "d", "e", "f")

expected <- data %>%
  tibble::as_tibble() %>%
  split(., .$time) %>%
  lapply(function(x) dplyr::select(x, -time))

test_that("collapsse_if_exploded on complete data", {
  test_gcl <- gridcoord::gcl_gridlist(data, space_coord, time_coord)
  res <- collapse_if_exploded(space_coord, time_coord, test_gcl)
  expect_equal(res, expected)
})

test_that("collapse_if_exploded does nothing on non-gridlists", {
  expect_equal(data, collapse_if_exploded(space_coord, time_coord, data))
  test <- gridcoord::gcl_gridlist(data, space_coord, time_coord) %>%
    gridcoord::gcl_as_list()
  expect_equal(test, collapse_if_exploded(space_coord, time_coord, test))
})

test_that("collapse_if_exploded on incomplete data", {
  test_gcl <- gridcoord::gcl_gridlist(data[c(1, 2, 5, 6), ], space_coord, time_coord)
  outcome <- expected
  outcome$t2 <- NA
  expect_equal(collapse_if_exploded(space_coord, time_coord, test_gcl), outcome)
})
