# Data and functions for testing extract_yhat.R

set.seed(615180269)

check_extract <- function(fit, yhat, f) {
  expect_true(".fitted" %in% colnames(yhat))
  expect_true(is.data.frame(yhat))
  # We need some way to check that our fitted values are actually reasonable.
  # We compare the means and also the range of values
  y <- yhat %>%
    dplyr::select(!!f[[2]]) %>%
    .[[1]]
  m1 <- mean(y)
  m2 <- mean(yhat$.fitted)
  d <- (m1-m2)/m2
  if (!"-" %in% as.character(f[[3]]))  {
    # Often get large disagreements if no intercept, which I don't understand but OK.

    if ("negbin" %in% class(fit) || grepl("qpois", as.character(f[[2]]))) {
      # Also get larger disagreemetns for glm.nb
      expect_true(abs(d) <= 0.04)
    } else {
      expect_true(abs(d) < 0.02)
    }
  }

  # Then check that the domains are correct
  fitted <- yhat$.fitted
  if (all(y <= 1) & all(y >= 0)) {
    expect_true(all(fitted >= 0) & all(fitted <= 1))
  }
  if (all(y > 0)) {
    expect_true(all(fitted > 0))
  }
}
compare_extract_aug <- function(fit, newdata, f) {

  yhat <- extract_yhat(fit, newdata)

  if (endsWith(class(fit)[[1]], "merMod")) {
    aug <- suppressWarnings(augment(fit, newdata = newdata, type.predict = "response"))
    aug <- dplyr::select(aug, id_time:.fitted)
  } else {
    aug <- augment(fit, newdata = newdata, type.predict = "response")
  }
  expect_equal(yhat, aug)
  check_extract(fit, yhat, f)
}

invlogit <- function (x) {
  1/(1 + exp(-x))
}

# Then create some basic data to test on:
start_date <- seq(lubridate::ymd("2010-01-01"), lubridate::ymd("2019-12-21"), by = "month")

spacetime_data <- expand.grid(id_time = seq_along(start_date),
                              id_space = 1:10) %>%
  dplyr::mutate(start_date = start_date[id_time]) %>%
  tibble::as_tibble()

n <- nrow(spacetime_data)
na_inds <- which(spacetime_data$id_time == max(spacetime_data$id_time))
good_inds <- seq_len(n)[-na_inds]

x_continuous <- runif(n, min = -5, max = 10) #continuous predictor
x_discrete <- sample(c("L1", "L2", "L3", "L4", "L5"), n, replace = TRUE) #discrete predictor
x_exp <- exp(runif(n, min = 0, max = 3.7))
x_binary <- rbinom(n, 1, 0.5)

exposure <- runif(n, min = 5, max = 200)
offset <- log(exposure)
factor_coeffs <- c(-2, 3, 1, 2, -1) %>%
  magrittr::set_names(sort(unique(x_discrete)))

space_coeffs <- rnorm(sd = 6, n = length(unique(spacetime_data$id_space)))

ey_newdata <- cbind(spacetime_data, x_continuous, x_discrete, x_exp, x_binary, exposure, offset) %>%
  dplyr::mutate(y_lm = 3 + 2.5 * x_continuous + rnorm(n, sd = 2),
                y_lm_f = 3 + 2.5 * x_continuous + factor_coeffs[x_discrete] + rnorm(n, sd= 2),
                y_logit = rbinom(n, 1, invlogit(x_exp * 0.05 + 1)),
                y_logit_f = rbinom(n, 1, invlogit(x_exp * 0.02 - factor_coeffs[x_discrete] / 2)),
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

ey_newdata_mermod <- cbind(spacetime_data, x_continuous, x_discrete, x_exp, x_binary, exposure, offset) %>%
  dplyr::mutate(y_lm = 3 + 2.5 * x_continuous + space_coeffs[.data$id_space] + rnorm(n, sd = 2),
                y_lm_f = 3 + 2.5 * x_continuous + factor_coeffs[x_discrete] +
                  space_coeffs[.data$id_space] +rnorm(n, sd= 2),
                y_logit = rbinom(n, 1, invlogit(x_exp * 0.05 +
                                                       space_coeffs[.data$id_space] + 1)),
                y_logit_f = rbinom(n, 1, invlogit(x_exp * 0.02 -
                                                         factor_coeffs[x_discrete] / 2 +
                                                         space_coeffs[.data$id_space])),
                y_pois = rpois(n, exp(2.8 + 0.012 * x_continuous) - 0.20 * x_binary +
                                 space_coeffs[.data$id_space]),
                y_pois_off = rpois(n, exp(-3 + 0.012 * x_continuous - 0.20 * x_binary +
                                            space_coeffs[.data$id_space]/3 + offset)),
                y_qpois_off = rnbinom(n,
                                      mu = exp(-3 + 0.012 * x_continuous - 0.20 * x_binary +
                                                 space_coeffs[.data$id_space]/3 + offset),
                                      size = 0.5))


ey_data_mermod <- ey_newdata_mermod %>%
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with("y_")),
                   function(x) {x[na_inds] <- NA; return(x)})


adj_f <- function(f) {
  f[[3]] <- rlang::expr(!!f[[3]] + (1 | id_space))
  f
}
formulas_mermod <- purrr::map(formulas, adj_f)

adj_f_inla <- function(f) {
  f[[3]] <- rlang::expr(!!f[[3]] + f(id_space, model = "iid"))
  f
}
formulas_inla <- purrr::map(formulas, adj_f_inla)
