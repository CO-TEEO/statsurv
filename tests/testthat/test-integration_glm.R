suppressWarnings(library("lubridate"))
suppressWarnings(library("forecast"))
suppressWarnings(library("dplyr"))
library(scanstatistics)
library(broom)

data("NM_data")
data("NM_county_sf")

test_that("Integration test for glm + NM_data", {
  skip_on_cran()
  NM_data <- tibble::as_tibble(NM_data)
  NM_data

  wd <- window_idtime(NM_data, min_train = 10, max_train = 10, n_predict = 1,
                      split_spatial_locations = FALSE)
  model_res <- wd %>%
    rowmute(training_data = prepare_training_data(curr_data, count, split_id, prep_strategy = "NA"),
            model_fit = glm(count ~ population, family = poisson, data = training_data),
            aug_data = extract_yhat(model_fit, curr_data))

  zones <- create_zones(NM_county_sf, max_k = 10, min_k = 2)
  zones <<- zones
  scan_results <- model_res %>%
    rowmute(scanres = scan_eb_poisson2(aug_data, count, zones, baseline_col = .fitted, n_mcsim = 49),
            scanres2 = remove_overlapping_clusters(scanres, zones, action_level, take_highest = TRUE),
            top_clusters = report_top_clusters(scanres2, action_level, max_reported = 3)) %>%
    dplyr::select(window_time_id, top_clusters) %>%
    tidyr::unnest(top_clusters)

  scan_results2 <- model_res %>%
    rowmute(scanres = scan_eb_poisson2(aug_data, count, zones, baseline_col = .fitted, n_mcsim = 49),
            scanres2 = remove_overlapping_clusters(scanres, zones, mc_pvalue, take_highest = FALSE),
            top_clusters = report_top_clusters(scanres2, order_by = mc_pvalue,
                                               take_highest = FALSE, cutoff_val = 0.05,
                                               max_reported = 3)) %>%
    dplyr::select(window_time_id, top_clusters) %>%
    tidyr::unnest(top_clusters)

  expect_true(is.data.frame(scan_results2))
})
