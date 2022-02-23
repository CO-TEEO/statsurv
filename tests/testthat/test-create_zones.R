library(sf)
data("NM_county_sf")

wanted_counties <- c("bernalillo", "valencia", "lincoln")
test_counties <- NM_county_sf %>%
  dplyr::filter(county %in% wanted_counties) %>%
  dplyr::transmute(id = dplyr::case_when(county == "bernalillo" ~ 1,
                                      county == "valencia" ~ 2,
                                      TRUE ~ 3)) %>%
  dplyr::arrange(id)

# 1 & 2 are very close to each other, 3 is further away (and closer to 2 than 1)

test_that("if max_k = 1, we get 1 per option", {
  expect_equal(create_zones(test_counties, max_k = 1), list(1, 2, 3))
})

test_that("if max_k = 2, we get 3 singles and 2 doubles", {
  expect_equal(create_zones(test_counties, max_k = 2), list(1, c(1, 2), 2, 3, c(2, 3)))
})

test_that("if max_k = 3, we get 3 singles, 2 doubles, and 1 triple", {
  expect_equal(create_zones(test_counties, max_k = 3), list(1, c(1, 2), c(1, 2, 3), 2, 3, c(2, 3)))
})


test_that("If max_k = 3 and min_k = 2, we get 2 doubles and 1 triple", {
  expect_equal(create_zones(test_counties, max_k = 3, min_k = 2), list(c(1, 2), c(1, 2, 3), c(2, 3)))
})

test_that("If max_k = 3 and min_k = 3, we get 1 triple", {
  expect_equal(create_zones(test_counties, max_k = 3, min_k = 3), list(c(1, 2, 3)))
})

test_that("We can work in different coordinate systems", {
  v1 <- create_zones(test_counties, max_k = 3)
  v2 <- create_zones(sf::st_transform(test_counties, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lon_0=-96.0 +datun=NAD83 +ellps=GRS80 +lat_0=23.0"), max_k = 3)
  v3 <- create_zones(sf::st_transform(test_counties, "+proj=utm +zone=13"), max_k = 3)
  expect_equal(v1, v2)
  expect_equal(v2, v3)
})

test_that("Passing something other than an sf or sfc gives an error", {
  df <- sf::st_drop_geometry(test_counties)
  expect_error(create_zones(df, 3), "sf")

  sfc <- sf::st_geometry(test_counties)
  expect_error(create_zones(sfc, 3), "sf")
})

