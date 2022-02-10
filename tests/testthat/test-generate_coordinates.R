# suppressWarnings(library("sf"))
#
# dmb <- rgdal::readOGR(file.path("denver_metro_boundary",
#                                 "denver_metro_boundary.shp"),
#                       verbose = FALSE,
#                       stringsAsFactors = FALSE)
#
# test_that("generate_study_area hasn't changed", {
#   expect_known_value(generate_study_area(dmb, "co_county_fips_2019", 0.2),
#                      "generate_study_area_results")
# })
#
# all_coords_match <- function(spdf_boundary, spdf_division, ...) {
#   sf_boundary <- sf::st_as_sf(spdf_boundary)
#   sf_division <- sf::st_as_sf(spdf_division)
#   res1 <- generate_study_area(spdf_boundary, spdf_division, ...)
#   res2 <- generate_study_area(sf_boundary, sf_division, ...)
#   expect_equivalent(generate_study_area(sf_boundary, spdf_division, ...),
#                     res1)
#   expect_equivalent(generate_study_area(spdf_boundary, sf_division, ...),
#                     res1)
#   expect_equivalent(generate_study_area(sf_boundary, sf_division, ...),
#                     res1)
# }
# # Ok, what would I want to test:
# # If boundary = NULL, uses the state of CO (check)
# #   Those results don't change depending on inside_fraction_area (check)
# # We can use any of the 4 named datasets, and we get reasonable results (check)
# # We can supply our own dataset... (need to put one of those in the tests folder)
#
# test_that("We can supply our own spdfs to generate_study_area", {
#   # A spdf with the boundaries of 3 zip codes: 80203, 80401, and 80746
#   test_path <- file.path("three_zips","three_zips.shp")
#   test_spdf <- rgdal::readOGR(test_path,
#                               verbose = FALSE,
#                               stringsAsFactors = FALSE)
#
#   expect_equivalent(generate_study_area(space_division = test_spdf),
#                     test_spdf)
#   output <- generate_study_area(dmb, test_spdf, inside_fraction_cutoff = 0)
#   expect_equal(output@data, data.frame(zcta_str = c("80203", "80401"), stringsAsFactors = FALSE))
#   all_coords_match(dmb, test_spdf, inside_fraction_cutoff = 0)
#
#   output <- generate_study_area(dmb, test_spdf, inside_fraction_cutoff = 1)
#   expect_equal(output@data, data.frame(zcta_str = c("80203"), stringsAsFactors = FALSE))
#   all_coords_match(dmb, test_spdf, inside_fraction_cutoff = 1)
# })
#
# test_that("generate_study_area works if surveillance_boundary = NULL", {
#   expect_equal(generate_study_area(space_division = "co_hsr_2019"), co_hsr_2019)
#   expect_equal(generate_study_area(space_division = "co_hsr_2019", inside_fraction_cutoff = 1),
#                co_hsr_2019)
#
#   expect_equal(generate_study_area(space_division = "denver_censustract_fips_2019"),
#                denver_censustract_fips_2019)
#   expect_equal(generate_study_area(space_division = "denver_censustract_fips_2019",
#                                    inside_fraction_cutoff = 1),
#                denver_censustract_fips_2019)
#
#   expect_equal(generate_study_area(space_division = "co_county_fips_2019"),
#                co_county_fips_2019)
#   expect_equal(generate_study_area(space_division = "co_county_fips_2019",
#                                    inside_fraction_cutoff = 1),
#                co_county_fips_2019)
#
#   expect_equal(generate_study_area(space_division = "co_state_boundary"), co_state_boundary)
#   expect_equal(generate_study_area(space_division = "co_state_boundary",
#                                    inside_fraction_cutoff = 1),
#                co_state_boundary)
# })
#
# test_that("Limiting to the greater denver metro area gives only 1 or 2 options", {
#   output <- generate_study_area(dmb, space_division = "co_hsr_2019", inside_fraction_cutoff = 0.6)
#   expect_equal(output@data, data.frame(hsr = "hsr_20", stringsAsFactors = FALSE))
#
#
#   output <- generate_study_area(dmb,
#                                 space_division = "co_county_fips_2019", inside_fraction_cutoff = 0.6)
#   # 08014 = Broomfield, 08031 = Denver
#   expect_equal(output@data, data.frame(fips_str = c("08014", "08031"), stringsAsFactors = FALSE))
#
#   res1 <- output
#   sf_dmb <- sf::st_as_sf(dmb)
#   res2 <- generate_study_area(sf_dmb,
#                               "co_county_fips_2019",
#                               inside_fraction_cutoff = 0.6)
#   expect_equivalent(res1, res2)
# })
#
#
# test_that("generate_date_range works for months, quarters, and years", {
#   output <- generate_date_range(lubridate::ymd("20180101"),
#                                 lubridate::ymd("20180331"), time_division = "month")
#   expect_equal(nrow(output), 3)
#   expect_equal(output[[2]], lubridate::ymd("20180101", "20180201", "20180301"))
#   expect_equal(output[[3]], lubridate::ymd("20180201", "20180301", "20180401"))
#
#
#   output <- generate_date_range(lubridate::ymd("20180101"),
#                                 lubridate::ymd("20180331"), time_division = "quarter")
#   expect_equal(nrow(output), 1)
#   expect_equal(output[[2]], lubridate::ymd("20180101"))
#   expect_equal(output[[3]], lubridate::ymd("20180401"))
#
#   output <- generate_date_range(lubridate::ymd("20180101"),
#                                 lubridate::ymd("20191231"), time_division = "year")
#   expect_equal(nrow(output), 2)
#   expect_equal(output[[2]], lubridate::ymd("20180101", "20190101"))
#   expect_equal(output[[3]], lubridate::ymd("20190101", "20200101"))
# })
#
# test_that("generate_date_range is inclusive", {
#   output <- generate_date_range(lubridate::ymd("20180101"),
#                                 lubridate::ymd("20180331"), "year")
#   expect_equivalent(output[1, c(2, 3)],
#                     data.frame(lubridate::ymd("20180101"), lubridate::ymd("20190101")))
#
# })
#
# test_that("generate_date_range works with weeks", {
#   output <- generate_date_range(lubridate::ymd("2012-01-01"),
#                                 lubridate::ymd("2012-12-31"), "week")
#   expect_equal(output[1, 1], "2012-W01")
#   expect_equal(output[1, 2], lubridate::ymd("2012-01-01"))
#   nr <- nrow(output)
#   expect_equal(output[nr, 1], "2013-W01")
#   expect_equal(output[nr, 2], lubridate::ymd("2012-12-30"))
#   expect_equal(output[nr, 3], lubridate::ymd("2013-01-06"))
#
#   output <- generate_date_range(lubridate::ymd("2012-01-01"),
#                                 lubridate::ymd("2012-12-31"), "week", week_start = 1)
#   expect_equal(output[1, 1], "2011-W52")
#   expect_equal(output[1, 2], lubridate::ymd("2011-12-26"))
#   nr <- nrow(output)
#   expect_equal(output[nr, 1], "2013-W01")
#   expect_equal(output[nr, 2], lubridate::ymd("2012-12-31"))
#   expect_equal(output[nr, 3], lubridate::ymd("2013-01-07"))
#
#   expect_error(generate_date_range(lubridate::ymd("2012-01-01"),
#                                    lubridate::ymd("2012-04-04"), "week", week_start = 2))
# })
#
# test_that("generate_date_range works with days", {
#   output <- generate_date_range(lubridate::ymd("2011-12-30"),
#                                 lubridate::ymd("2012-01-02"), "day")
#   expect_equal(output[1, 1], "2011-12-30")
#   expect_equal(nrow(output), 4)
#   expect_equal(output[4, 3], lubridate::ymd("2012-01-03"))
#   expect_equal(output[4, 2], lubridate::ymd("2012-01-02"))
# })
#
# test_that("We can set the coordinate name to be whatever we want", {
#   output <-  generate_date_range(lubridate::ymd("20180101"),
#                                  lubridate::ymd("20180331"),
#                                  time_division = "month")
#   expect_equal(colnames(output)[[1]], "date_label")
#   output <-  generate_date_range(lubridate::ymd("20180101"),
#                                  lubridate::ymd("20180331"),
#                                  time_division = "month",
#                                  coordinate_name = "new_name")
#   expect_equal(colnames(output)[[1]], "new_name")
# })
#
#
# test_that("coordinate_name works with non-syntactic names", {
#   output <-  generate_date_range(lubridate::ymd("20180101"),
#                                  lubridate::ymd("20180331"),
#                                  time_division = "month",
#                                  coordinate_name = "%badname huh?")
#   expect_equal(colnames(output)[[1]], "%badname huh?")
# })
