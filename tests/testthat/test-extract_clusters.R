# What do I want to test here?
# 1. Works with multiple different alarm statistics (bayes, eb_poisson, eb_poisson_fast)
# 2. Cutoff behaves appropriately - upper bound for mc_pvalue, lower bound for et else
# 3. Max allowed behaves correctly - we never get more than max allowed results
# 4. Allow overlap works correctly - we never have the same zone listed more than once, and if allow
#    overlap = FALSE, we never see the same location more than once.

# Set up data to run scan functions on:
library("scanstatistics")
library("sf")
data(NM_popcas)
NM_popcas$county <- as.character(NM_popcas$county)
nm_county_coord <- statsurv::nm_county_coord

year_coord <- generate_date_range(lubridate::ymd("1973-01-01"),
                                  lubridate::ymd("1991-01-01"),
                                  time_division = "year",
                                  coordinate_name = "year")


fit <-  glm(count ~ year,
            family = poisson(link = "log"),
            offset = log(population),
            data = NM_popcas)

yhat <- extract_yhat(nm_county_coord, year_coord, fit, NM_popcas)

wide_cases <- pivot_for_scan(NM_popcas, "count", nm_county_coord, year_coord)
wide_bases <- pivot_for_scan(yhat, "yhat", nm_county_coord, year_coord)

zones <- space_coord_to_zones(nm_county_coord, 16)

check_overlap <- function(clusters, zones, allow_overlap = TRUE) {
  if (allow_overlap) {
    expect_true(anyDuplicated(clusters$zone) == 0)
  } else {
    expect_true(anyDuplicated(unlist(zones[clusters$zone])) == 0)
  }
}

check_n_returned <- function(clusters, n) {
  expect_true(nrow(clusters) > 0 && nrow(clusters) <= n)
}

check_cutoff <- function(clusters, cutoff, use_column) {
  if (nrow(clusters) > 1) {
    if (use_column == "mc_pvalue") {
      expect_true(max(clusters[[use_column]]) <= cutoff)
    } else {
      expect_true(min(clusters[[use_column]]) >= cutoff)
    }
  }
}

extract_and_check <- function(scanres, zones, use_column, cutoff, max_allowed, overlap) {
  clust <- extract_clusters(scanres, zones, use_column, cutoff, max_allowed, overlap)
  check_n_returned(clust, max_allowed)
  check_overlap(clust, zones, overlap)
  check_cutoff(clust, cutoff, use_column)
  return(clust)
}

test_that("Works on non-standard ebp", {
  scanres_ebp <- scan_eb_poisson(wide_cases, zones, wide_bases, n_mcsim = 100)
  extract_and_check(scanres_ebp, zones, "score", cutoff = 5, max_allowed = 16, overlap = TRUE)
  extract_and_check(scanres_ebp, zones, "score", cutoff = 1, max_allowed = 16, overlap = TRUE)
  extract_and_check(scanres_ebp, zones, "score", cutoff = 1, max_allowed = 4, overlap = TRUE)
  extract_and_check(scanres_ebp, zones, "score", cutoff = 0.03, max_allowed = 4, overlap = FALSE)
  extract_and_check(scanres_ebp, zones, "mc_pvalue", cutoff = 0.05, max_allowed = 4, overlap = FALSE)
  expect_error(extract_clusters(scanres_ebp, zones, "action_level"))
  expect_error(extract_clusters(scanres_ebp, zones, "log_posterior"))
  expect_error(extract_clusters(scanres_ebp, zones, "score", max_allowed = -4))
})

test_that("Works on non-standard ebp_fast", {
  scanres_ebpf <- scan_eb_poisson_fast(wide_cases, zones, wide_bases, n_mcsim = 100)
  extract_and_check(scanres_ebpf, zones, "score", cutoff = 5, max_allowed = 16, overlap = TRUE)
  extract_and_check(scanres_ebpf, zones, "score", cutoff = 1, max_allowed = 16, overlap = TRUE)
  extract_and_check(scanres_ebpf, zones, "score", cutoff = 1, max_allowed = 4, overlap = TRUE)
  extract_and_check(scanres_ebpf, zones, "score", cutoff = 0.03, max_allowed = 4, overlap = FALSE)
  extract_and_check(scanres_ebpf, zones, "mc_pvalue", cutoff = 0.05, max_allowed = 4, overlap = FALSE)
  expect_error(extract_clusters(scanres_ebpf, zones, "action_level"))
  expect_error(extract_clusters(scanres_ebpf, zones, "log_posterior"))
  expect_error(extract_clusters(scanres_ebpf, zones, "score", max_allowed = -4))
})

test_that("Works on bayes negbin", {
  scanres_bnb <- scan_bayes_negbin(wide_cases, zones, wide_bases)
  extract_and_check(scanres_bnb, zones, "log_posterior", cutoff = 5, max_allowed = 16, overlap = TRUE)
  extract_and_check(scanres_bnb, zones, "log_posterior", cutoff = 1, max_allowed = 16, overlap = TRUE)
  extract_and_check(scanres_bnb, zones, "log_posterior", cutoff = 1, max_allowed = 4, overlap = TRUE)
  extract_and_check(scanres_bnb, zones, "log_posterior", cutoff = 0.03, max_allowed = 4, overlap = FALSE)
  expect_error(extract_clusters(scanres_bnb, zones, "action_level"))
  expect_error(extract_clusters(scanres_bnb, zones, "score"))
  expect_error(extract_clusters(scanres_bnb, zones, "mc_pvalue"))
  expect_error(extract_clusters(scanres_bnb, zones, "log_posterior", max_allowed = -4))
})

test_that("works on standardize_alarm", {
  scanres_std <- standardized_alarm_functions("scan_bayes_negbin",
                                           wide_cases,
                                           zones,
                                           wide_bases,
                                           n_mcsim = 100)
  extract_and_check(scanres_std, zones, "action_level", cutoff = 5, max_allowed = 16, overlap = TRUE)
  extract_and_check(scanres_std, zones, "action_level", cutoff = 1, max_allowed = 16, overlap = TRUE)
  extract_and_check(scanres_std, zones, "action_level", cutoff = 1, max_allowed = 4, overlap = TRUE)
  extract_and_check(scanres_std, zones, "action_level", cutoff = 0.03, max_allowed = 4, overlap = FALSE)
  expect_error(extract_clusters(scanres_std, zones, "score", max_allowed = -4))
})

test_that("Oddities on replicates give reasonable errors", {
  scanres_ebp <- scan_eb_poisson(wide_cases, zones, wide_bases, n_mcsim = 0)
  expect_error(extract_clusters(scanres_ebp, zones, "mc_pvalue"),
               "replicates")
  scanres_std <- standardized_alarm_functions("scan_eb_poisson",
                                              wide_cases,
                                              zones,
                                              wide_bases,
                                              n_mcsim = 100)
  extract_and_check(scanres_std, zones, "mc_pvalue", cutoff = 0.01, max_allowed = 16, TRUE)
})

