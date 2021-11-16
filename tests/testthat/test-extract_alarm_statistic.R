set.seed(2311212)

space_coord <- rgdal::readOGR("three_zips/three_zips.shp",
                              verbose = FALSE,
                              stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()

start_times <- 1:12
fin_times <- start_times + 1L
labels <- paste0("X", start_times)
time_coord <- data.frame(time_labels = labels,
                         time = start_times,
                         fin_time = fin_times,
                         stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()

data_for_scan <- gridcoord::gc_expand(time_coord, space_coord)
data_for_scan$baseline <- 4.3
data_for_scan$baseline2 <- 6
is_outbreak <- data_for_scan$zcta_str %in% c("80401", "80203") & data_for_scan$time >= 9
data_for_scan$observed <- floor(data_for_scan$baseline + ifelse(is_outbreak, 4, 0))
null_f <- function(space_coord, time_coord, data_for_model) {
  return(list(fit = 0, data = data_for_model))
}
all_ret <- loop_model(space_coord, time_coord, data_for_scan, "observed", null_f, use_cache = FALSE)
all_data_for_scan <- all_ret[[2]]
all_yhats <- lapply(all_data_for_scan,
                    function(x) {
                      if (identical(x, NA)) {
                        x
                      } else {
                        x[, c("time_labels", "zcta_str", "baseline", "baseline2")]
                      }
                    })

check_all_coord_types <- function(spdf_coord, span_coord, all_alarms, zone = NULL) {
  expect_error(res1 <- extract_alarm_statistic(spdf_coord, span_coord, all_alarms, zone),
               NA)
  sf_coord <- sf::st_as_sf(spdf_coord)
  point_space_coord <- sf::st_drop_geometry(sf_coord)
  point_time_coord <- span_coord[, 1, drop = FALSE]

  expect_error(res2 <- extract_alarm_statistic(sf_coord, span_coord, all_alarms, zone),
               NA)
  expect_error(res3 <- extract_alarm_statistic(point_space_coord, span_coord, all_alarms, zone),
               NA)
  expect_error(res4 <- extract_alarm_statistic(spdf_coord, point_time_coord, all_alarms, zone),
               NA)
  expect_error(res5 <- extract_alarm_statistic(point_space_coord, point_time_coord, all_alarms, zone),
               NA)
  expect_error(res6 <- extract_alarm_statistic(sf_coord, point_time_coord, all_alarms, zone),
               NA)

  expect_equal(res1, res2)
  expect_equal(res1, res3)
  expect_equal(res1[, 1:8], res4)
  expect_equal(res1[, 1:8], res5)
  expect_equal(res1[, 1:8], res6)
}

check_both_zones <- function(spdf_coord, span_coord, all_alarms, zone) {
  check_all_coord_types(spdf_coord, span_coord, all_alarms, zone = zone)
  check_all_coord_types(spdf_coord, span_coord, all_alarms)
}
test_that("Scan alarm w/ observed and replicates", {
  all_alarms <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                    "scan_eb_poisson", max_k = 2, use_cache = FALSE, n_mcsim = 10)

  check_both_zones(space_coord, time_coord, all_alarms, zone = 2)
})

test_that("Scan alarm w/o replicates", {
  all_alarms <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                    "scan_eb_poisson", max_k = 2, use_cache = FALSE, n_mcsim = 0)

  check_both_zones(space_coord, time_coord, all_alarms, zone = 2)
})

test_that("Scan alarm on bayes-type scan", {
  all_alarms <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                    "scan_bayes_negbin", max_k = 2, use_cache = FALSE)

  check_both_zones(space_coord, time_coord, all_alarms, zone = 2)
})

test_that("Scan alarm on scan_eb_poisson_fast", {
  all_alarms <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                    "scan_eb_poisson_fast", max_k = 2, use_cache = FALSE,
                                    n_mcsim = 10)

  check_both_zones(space_coord, time_coord, all_alarms, zone = 2)

  all_alarms <- loop_alarm_function(space_coord, time_coord, all_yhats, all_data_for_scan, "observed",
                                    "scan_eb_poisson_fast", max_k = 2, use_cache = FALSE,
                                    n_mcsim = 0)

  check_both_zones(space_coord, time_coord, all_alarms, zone = 2)
})

test_that("Scan alarm on non-standardized alarm statistics", {
  # Now, how do we make a list of non-standard alarm statistics?
  all_wide_cases <- loop_over(space_coord, time_coord, all_data_for_scan,
                              function(x) pivot_for_scan(x, "observed", space_coord, time_coord))
  all_wide_bases <- loop_over(space_coord, time_coord, all_yhats,
                              function(x) pivot_for_scan(x, "baseline", space_coord, time_coord))
  zones <- space_coord_to_zones(space_coord, max_k = 2)

  all_scan_alarms_ebp <- all_wide_cases
  all_scan_alarms_edpf <- all_wide_cases
  all_scan_alarms_bayes <- all_wide_cases
  for (nm in names(all_wide_cases)) {
    if (identical(all_wide_cases[[nm]], NA)) {
      next
    }
    all_scan_alarms_ebp[[nm]] <- scanstatistics::scan_eb_poisson(all_wide_cases[[nm]],
                                             zones,
                                             all_wide_bases[[nm]],
                                             n_mcsim = 10)
    all_scan_alarms_edpf[[nm]] <- scan_eb_poisson_fast(all_wide_cases[[nm]],
                                                       zones,
                                                       all_wide_bases[[nm]],
                                                       n_mcsim = 10)
    all_scan_alarms_bayes[[nm]] <- scanstatistics::scan_bayes_negbin(all_wide_cases[[nm]],
                                                     zones,
                                                     all_wide_bases[[nm]])
  }

  check_both_zones(space_coord, time_coord, all_scan_alarms_ebp, zone = 2)
  check_both_zones(space_coord, time_coord, all_scan_alarms_edpf, zone = 2)
  check_both_zones(space_coord, time_coord, all_scan_alarms_bayes, zone = 2)
})

