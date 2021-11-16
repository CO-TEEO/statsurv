start_times <- 1:12
fin_times <- start_times + 1L
labels <- paste0("X", start_times)
time_coord <- data.frame(time_labels = labels,
                         time = start_times,
                         fin_time = fin_times,
                         stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()

spdf_coord <- rgdal::readOGR("three_zips/three_zips.shp",
                             verbose = FALSE,
                             stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()

test_that("space_coord_into_gridlist", {
  res <- space_coord_into_gridlist(spdf_coord, spdf_coord, time_coord)
  expect_equal(dim(res), c(nrow(spdf_coord), nrow(time_coord)))
  lapply(res[1, , drop = TRUE], expect_equal, spdf_coord[1, ])
  lapply(res[2, , drop = TRUE], expect_equal, spdf_coord[2, ])
  lapply(res[3, , drop = TRUE], expect_equal, spdf_coord[3, ])
})

test_that("space_coord_into_gridlist, stub-space", {
  res <- space_coord_into_gridlist(spdf_coord, gridcoord::gc_stubcoord(), time_coord)
  expect_equal(dim(res), c(1, nrow(time_coord)))
  lapply(res[1, , drop = TRUE], expect_equal, spdf_coord)
})

test_that("space_coord_into_gridlist, 1-space", {
  one_zip <- spdf_coord[1, ]
  res <- space_coord_into_gridlist(one_zip, one_zip, time_coord)
  expect_equal(dim(res), c(1, nrow(time_coord)))
  lapply(res[1, , drop = TRUE], expect_equal, one_zip)
})

test_that("space_coord_into_gridlist, 1-space, 1-time", {
  one_zip <- spdf_coord[2, ]
  one_time <- time_coord[6, ]
  res <- space_coord_into_gridlist(one_zip, one_zip, one_time)
  expect_equal(dim(res), c(1, 1))
  expect_equal(res[[1, 1]], one_zip)
})

test_that("pad_with_nas", {
  res <- pad_with_nas(list(X1 = 1, X12 = 2), time_coord)
  expect_equal(unname(unlist(res)), c(1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2))

  res <- pad_with_nas(list(X1 = 1), time_coord[1, , drop = FALSE])
  expect_equal(res, list(X1 = 1))

  res <- pad_with_nas(list(), time_coord[1:3, ])
  expect_equal(res, list(X1 = NA, X2 = NA, X3 = NA))
})

