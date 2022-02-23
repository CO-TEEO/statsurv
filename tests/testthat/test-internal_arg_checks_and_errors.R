# test_that("error_type throws good errors", {
#   bananas <- "fruit"
#   expect_error(error_type(bananas, "numeric"),
#                "bananas",
#                class = "error_type")
#   expect_error(error_type(bananas, "numeric", "pineapple"),
#                "pineapple",
#                class = "error_type")
#
#
#   expect_error(check_type(bananas, "numeric"),
#                "bananas",
#                class = "error_type")
#
#   expect_error(check_type(bananas, c("numeric", "data.frame")),
#                "bananas",
#                class = "error_type")
#
#   expect_error(check_scalar_type(bananas, c("numeric", "data.frame")),
#                "bananas",
#                class = "error_type")
# })
#
# test_that("error_scalar throws good errors", {
#   avocado <- c("One", "Two", "Three")
#   expect_error(error_scalar(avocados),
#                "avocado",
#                class = "error_scalar")
#   expect_error(error_scalar(avocados, "fir"),
#                "fir",
#                class = "error_scalar")
#
#   expect_error(check_scalar(avocado),
#                "avocado",
#                class = "error_scalar")
#
#   expect_error(check_scalar_type(avocado, "character"),
#                "avocado",
#                class = "error_scalar")
# })
#
#
