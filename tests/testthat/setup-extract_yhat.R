# Data and functions for testing extract_yhat.R

set.seed(615180269)

rowMedians <- function(data) {
  apply(data, 1, median)
}

expect_similar <- function(object, object2, pcnt_err = 0.05, abs_err = 0.2) {
  act <- quasi_label(rlang::enquo(object), arg = "object")
  act2 <- quasi_label(rlang::enquo(object2), arg = "object2")
  object <- unname(object)
  object2 <- unname(object2)
  wi_pcnt <- abs((object - object2) / object) <= pcnt_err
  wi_abs <- abs(object - object2) <= abs_err
  if (all(wi_pcnt | wi_abs)) {
    succeed()
    return(invisible(act$val))
  }

  good_m <- wi_abs | wi_pcnt
  bad_m <- !good_m
  msg <- compare(object[bad_m], object2[bad_m])$message
  fail(msg)
}

compare_extract_aug <- function(fit, newdata) {
  yhat <- extract_yhat(fit, newdata)
  aug <- augment(fit, newdata = newdata)
  expect_equal(yhat, aug)
  expect_true(".fitted" %in% colnames(yhat))
  expect_true(is.data.frame(yhat))
}

# check_yhat_means <- function(space_coord, time_coord, fit, data, n_samples = 100, tol = 0.01) {
#   yhat <- extract_yhat(space_coord, time_coord, fit, data)
#   test_that("extract_yhat gives a data frame", {expect_true(is.data.frame(yhat))})
#   row_has_na <- apply(data, 1, function(x) {any(is.na(x))})
#   non_na_inds <- which(row_has_na == FALSE)
#   test_that("extract_yhat matches predict", {
#     expect_equal(yhat[[3]][non_na_inds], unname(predict(fit, type = "response")))
#   })
#   test_that("sample_yhat converges to extract_yhat", {
#     samples <- sample_yhat(space_coord, time_coord, fit, data, n_samples = n_samples)
#     expect_similar(rowMedians(samples[, 3:ncol(samples)]), yhat[[3]])
#   })
# }

#
# check_inla <- function(space_coord,
#                        time_coord,
#                        fit,
#                        data,
#                        expected,
#                        tol = 1e-4,
#                        invlink = arm::invlogit,
#                        n_samples = 100) {
#
#   test_that("Of class INLA", {
#     expect_true("inla" %in% class(fit))
#   })
#
#   yhat <- extract_yhat(space_coord, time_coord, fit, data)
#   test_that("extract_yhat gives a data frame", {expect_true(is.data.frame(yhat))})
#   row_has_na <- apply(data, 1, function(x) {any(is.na(x))})
#   non_na_inds <- which(row_has_na == FALSE)
#
#   test_that("extract_yhat.INLA matches expected", {
#     expect_equal(yhat[[3]],
#                  expected,
#                  max_diffs = 14,
#                  tolerance = tol)
#   })
#   test_that("sample_yhat.INLA coverges to extract_yhat.INLA", {
#     samples <- suppressWarnings(sample_yhat(space_coord, time_coord, fit, data, n_samples))
#     expect_similar(rowMeans(samples[, 3:ncol(samples)]), yhat[[3]])
#   })
# }
#
# check_mermod <- function(space_coord, time_coord, fit, data, tol = 0.01) {
#   yhat <- extract_yhat(space_coord, time_coord, fit, data)
#   test_that("extract_yhat gives a data frame", {expect_true(is.data.frame(yhat))})
#   row_has_na <- apply(data, 1, function(x) {any(is.na(x))})
#   non_na_inds <- which(row_has_na == FALSE)
#   test_that("extract_yhat matches predict", {
#     expect_equal(yhat[[3]][non_na_inds], unname(predict(fit, type = "response")))
#   })
# }

# Then create some basic data to test on:
# id_time <- seq(1:100)
time_coord <- generate_date_range(lubridate::ymd("2010-01-01"),
                                  lubridate::ymd("2019-12-21"),
                                  time_division = "month") %>%
  tibble::as_tibble()

spacetime_data <- expand.grid(id_time = seq_len(nrow(time_coord)),
                              id_space = 1:10) %>%
  dplyr::mutate(start_date = time_coord$start_date[id_time])

n <- nrow(spacetime_data)
na_inds <- which(spacetime_data$id_time == max(spacetime_data$id_time))
good_inds <- seq_len(n)[-na_inds]

x_continuous <- runif(n, min = -5, max = 10) #continuous predictor
x_discrete <- sample(c("L1", "L2", "L3", "L4", "L5"), n, replace = TRUE) #discrete predictor
x_exp <- exp(runif(n, min = 0, max = 5.7))
x_binary <- rbinom(n, 1, 0.5)

exposure <- runif(n, min = 5, max = 200)
offset <- log(exposure)
factor_coeffs <- c(-2, 3, 1, 2, -1) %>%
  magrittr::set_names(sort(unique(x_discrete)))

space_coeffs <- rnorm(sd = 6, n = length(unique(spacetime_data$id_space)))

ey_newdata <- cbind(spacetime_data, x_continuous, x_discrete, x_exp, x_binary, exposure, offset) %>%
  dplyr::mutate(y_lm = 3 + 2.5 * x_continuous + rnorm(n, sd = 2),
                y_lm_f = 3 + 2.5 * x_continuous + factor_coeffs[x_discrete] + rnorm(n, sd= 2),
                y_logit = rbinom(n, 1, arm::invlogit(x_exp * 0.05 + 1)),
                y_logit_f = rbinom(n, 1, arm::invlogit(x_exp * 0.02 - factor_coeffs[x_discrete] / 2)), #Testing changing 0.05 to 0.02
                y_pois = rpois(n, exp(2.8 + 0.012 * x_continuous) - 0.20 * x_binary),
                y_pois_off = rpois(n, exp(-3 + 0.012 * x_continuous - 0.20 * x_binary + offset)),
                y_qpois_off = rnbinom(n,
                                      mu = exp(-3 + 0.012 * x_continuous - 0.20 * x_binary + offset),
                                      size = 0.5),
                y_varint = 3 + 2.5 * x_continuous + space_coeffs[.data$id_space] + rnorm(n, sd = 3))

ey_data <- ey_newdata %>%
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with("y_")),
                   function(x) {x[na_inds] <- NA; return(x)})




formulas <- list(
  f_lm = y_lm ~ x_continuous,
  f_lm_noint = y_lm ~ x_continuous - 1,
  f_lm_f = y_lm_f ~ x_continuous + x_discrete,
  f_logit = y_logit ~ x_exp,
  f_logit_trans = y_logit ~ log(x_exp),
  f_logit_noint = y_logit ~ x_exp - 1,
  f_logit_f = y_logit_f ~ x_exp + x_discrete,
  f_logit_noint_f = y_logit_f ~ x_exp + x_discrete - 1,
  f_pois = y_pois ~ x_continuous + x_binary,
  f_pois_f = y_pois ~ x_continuous + factor(x_binary),
  f_pois_off_wo = y_pois_off ~ x_continuous + x_binary,
  f_pois_off_wi = y_pois_off ~ x_continuous + x_binary + offset(offset),
  f_qpois_off_wo = y_qpois_off ~ x_continuous + x_binary,
  f_qpois_off_wi = y_qpois_off ~ x_continuous + x_binary + offset(offset)
  )



