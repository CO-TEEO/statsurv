# convert_to_surveillance takes a set of data.frames and effectively pulls out the
# last n-rows in each one.

set.seed(986561351)

space_coord <- rgdal::readOGR("three_zips/three_zips.shp",
                              verbose = FALSE,
                              stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()

single_space <- space_coord[1, ]

time_coord <- data.frame(time = c(1, 2, 3, 4, 5),
                         start = seq(1.1, 5.1, by = 1),
                         fin = seq(1.9, 5.9, by = 1))


list_of_dfs <- lapply(1:5,
                      function(n) {
                        gridcoord::gc_expand(time_coord[seq(max(n-1, 1), n), 1 , drop = FALSE], space_coord) %>%
                          dplyr::mutate(obs = n + as.numeric(zcta_str)/1e5)
                      })

names(list_of_dfs) <- 1:5
list_of_dfs[[1]] <- NA
list_of_dfs[[2]] <- NA

check_all_coord_types <- function(spdf_coord, span_coord, all_dfs, ...) {
  expect_error(res1 <- convert_to_surveillance(spdf_coord, span_coord, all_dfs, ...),
               NA)
  sf_coord <- sf::st_as_sf(spdf_coord)
  point_space_coord <- sf::st_drop_geometry(sf_coord)
  point_time_coord <- span_coord[, 1, drop = FALSE]

  expect_error(res2 <- convert_to_surveillance(sf_coord, span_coord, all_dfs, ...),
               NA)
  expect_error(res3 <- convert_to_surveillance(point_space_coord, span_coord, all_dfs, ...),
               NA)
  expect_error(res4 <- convert_to_surveillance(spdf_coord, point_time_coord, all_dfs, ...),
               NA)
  expect_error(res5 <- convert_to_surveillance(point_space_coord, point_time_coord, all_dfs, ...),
               NA)
  expect_error(res6 <- convert_to_surveillance(sf_coord, point_time_coord, all_dfs, ...),
               NA)

  expect_equal(res1, res2)
  expect_equal(res1, res3)
  expect_equal(res1, res4)
  expect_equal(res1, res5)
  expect_equal(res1, res6)
}

test_that("convert_to_surveillance works", {
  res <- convert_to_surveillance(space_coord, time_coord, list_of_dfs, n_predict = 1) %>%
    loop_over(space_coord, time_coord, ., function(x) dplyr::arrange(x, time, zcta_str))

  expect_equal(res$`2`, NA)
  expect_equal(res$`5`$obs, c(4.80203, 4.80401, 4.80746, 5.80203, 5.80401, 5.80746))

  check_all_coord_types(space_coord, time_coord, list_of_dfs)
})

test_that("grow_length means we grow the length", {
  res <- convert_to_surveillance(space_coord, time_coord, list_of_dfs,
                                 n_predict = 1, grow_length = TRUE) %>%
    loop_over(space_coord, time_coord, ., function(x) dplyr::arrange(x, time, zcta_str))
  expect_equal(res$`2`, NA)
  expect_equal(res$`5`$obs, c(3.80203, 3.80401, 3.80746,
                              4.80203, 4.80401, 4.80746,
                              5.80203, 5.80401, 5.80746))

  check_all_coord_types(space_coord, time_coord, list_of_dfs)
})

test_that("return_last_only means we only return a single df", {
  res <- convert_to_surveillance(space_coord, time_coord, list_of_dfs,
                                 n_predict = 1,
                                 return_last_only = TRUE) %>%
     dplyr::arrange(time, zcta_str)
  expect_true(is.data.frame(res))
  expect_equal(res$obs, c(4.80203, 4.80401, 4.80746,
                          5.80203, 5.80401, 5.80746))

  res <- convert_to_surveillance(space_coord, time_coord, list_of_dfs,
                                 n_predict = 1,
                                 return_last_only = TRUE,
                                 grow_length = TRUE) %>%
    dplyr::arrange(time, zcta_str)

  expect_true(is.data.frame(res))
  expect_equal(res$obs, c(3.80203, 3.80401, 3.80746,
                              4.80203, 4.80401, 4.80746,
                              5.80203, 5.80401, 5.80746))

  check_all_coord_types(space_coord, time_coord, list_of_dfs,
                        return_last_only = TRUE,
                        grow_length = TRUE)

  check_all_coord_types(space_coord, time_coord, list_of_dfs,
                        return_last_only = TRUE,
                        grow_length = FALSE)
})

test_that("Works with n_predict != 1", {
  res <- convert_to_surveillance(space_coord, time_coord, list_of_dfs,
                                 n_predict = 2) %>%
    loop_over(space_coord, time_coord, ., function(x) dplyr::arrange(x, time, zcta_str))
  expect_equal(res$`5`$obs, c(5.80203, 5.80401, 5.80746,
                              5.80203, 5.80401, 5.80746))
  expect_equal(res$`2`, NA)
  check_all_coord_types(space_coord, time_coord, list_of_dfs,
                        n_predict = 2)

  res <- convert_to_surveillance(space_coord, time_coord, list_of_dfs,
                                n_predict = 2, grow_length = TRUE) %>%
    loop_over(space_coord, time_coord, ., function(x) dplyr::arrange(x, time, zcta_str))
  expect_equal(res$`5`$obs, c(3.80203, 3.80401, 3.80746,
                              4.80203, 4.80401, 4.80746,
                              5.80203, 5.80401, 5.80746,
                              5.80203, 5.80401, 5.80746))
  expect_equal(res$`2`, NA)
  check_all_coord_types(space_coord, time_coord, list_of_dfs,
                        n_predict = 2, grow_length = TRUE)

  res <- convert_to_surveillance(space_coord, time_coord, list_of_dfs,
                                 n_predict = 2, grow_length = TRUE,
                                 return_last_only = TRUE) %>%
    dplyr::arrange(time, zcta_str)
  expect_true(is.data.frame(res))
  expect_equal(res$obs, c(3.80203, 3.80401, 3.80746,
                          4.80203, 4.80401, 4.80746,
                          5.80203, 5.80401, 5.80746,
                          5.80203, 5.80401, 5.80746))
  check_all_coord_types(space_coord, time_coord, list_of_dfs,
                        n_predict = 2, grow_length = TRUE,
                        return_last_only = TRUE)


  res <- convert_to_surveillance(space_coord, time_coord, list_of_dfs,
                                 n_predict = 2, grow_length = FALSE,
                                 return_last_only = TRUE) %>%
    dplyr::arrange(time, zcta_str)
  expect_true(is.data.frame(res))
  expect_equal(res$obs, c(5.80203, 5.80401, 5.80746,
                          5.80203, 5.80401, 5.80746))
  check_all_coord_types(space_coord, time_coord, list_of_dfs,
                        n_predict = 2, grow_length = FALSE,
                        return_last_only = TRUE)
})


# start_times <- 1:12
# fin_times <- start_times + 1L
# labels <- paste0("X", start_times) #sapply(start_times + 64, intToUtf8)
# time_coord <- data.frame(time_labels = labels,
#                          time = start_times,
#                          fin_time = fin_times,
#                          stringsAsFactors = FALSE) %>%
#   gridcoord::gc_gridcoord()
#
# data_for_scan <- gridcoord::gc_expand(time_coord, space_coord)
# data_for_scan$baseline <- 4.3
# data_for_scan$baseline2 <- 6
# # is_outbreak <- #space2 and space3, 10-13
# is_outbreak <- data_for_scan$zcta_str %in% c("80401", "80203") & data_for_scan$time >= 9
# data_for_scan$observed <- floor(data_for_scan$baseline + ifelse(is_outbreak, 4, 0))
# null_f <- function(space_coord, time_coord, data_for_model) {
#   return(list(fit = 0, data = data_for_model))
# }
# all_ret <- loop_model(space_coord, time_coord, data_for_scan, "observed", null_f, use_cache = FALSE)
# all_data_for_scan <- all_ret[[2]]
# all_yhats <- lapply(all_data_for_scan,
#                     function(x) {
#                       if (identical(x, NA)) {
#                         x
#                       } else {
#                         x[, c("time_labels", "zcta_str", "baseline", "baseline2")]
#                       }
#                     })
#

