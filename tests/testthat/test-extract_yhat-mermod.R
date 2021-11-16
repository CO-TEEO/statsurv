# na_inds <- apply(ey_data, 1, function(x) {any(is.na(x))})
#
# space_coeffs <- space_coord %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(space_coeffs = rnorm(sd = 6, n = 1)) %>%
#   .$space_coeffs %>%
#   magrittr::set_names(gridcoord::gc_get_labels(space_coord))

data_mermod <- cbind(comb_coords, x_continuous, x_discrete, x_exp, x_binary, exposure, offset) %>%
  dplyr::mutate(y_lm = 3 + 2.5 * x_continuous + space_coeffs[.data$space_reg] + rnorm(n, sd = 2),
                y_lm_f = 3 + 2.5 * x_continuous + factor_coeffs[x_discrete] + space_coeffs[.data$space_reg] +rnorm(n, sd= 2),
                y_logit = rbinom(n, 1, arm::invlogit(x_exp * 0.05 + space_coeffs[.data$space_reg] + 1)),
                y_logit_f = rbinom(n, 1, arm::invlogit(x_exp * 0.02 - factor_coeffs[x_discrete] / 2 + space_coeffs[.data$space_reg])),
                y_pois = rpois(n, exp(2.8 + 0.012 * x_continuous) - 0.20 * x_binary + space_coeffs[.data$space_reg]),
                y_pois_off = rpois(n, exp(-3 + 0.012 * x_continuous - 0.20 * x_binary + space_coeffs[.data$space_reg]/3 + offset)),
                y_qpois_off = rnbinom(n,
                                      mu = exp(-3 + 0.012 * x_continuous - 0.20 * x_binary + space_coeffs[.data$space_reg]/3 + offset),
                                      size = 0.5)) %>%
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with("y_")),
                   function(x) {x[na_inds] <- NA; return(x)})

formulas_mermod <- list(
  f_lm = y_lm ~ x_continuous + (1 | space_reg),
  f_lm_noint = y_lm ~ x_continuous - 1 + (1 | space_reg),
  f_lm_f = y_lm_f ~ x_continuous + x_discrete + (1 | space_reg),
  f_logit = y_logit ~ x_exp + (1 | space_reg),
  f_logit_trans = y_logit ~ log(x_exp) + (1 | space_reg),
  f_logit_noint = y_logit ~ x_exp - 1 + (1 | space_reg),
  f_logit_f = y_logit_f ~ x_exp + x_discrete + (1 | space_reg),
  f_logit_noint_f = y_logit_f ~ x_exp + x_discrete - 1 + (1 | space_reg),
  f_pois = y_pois ~ x_continuous + x_binary + (1 | space_reg),
  f_pois_f = y_pois ~ x_continuous + factor(x_binary) + (1 | space_reg),
  f_pois_off_wo = y_pois_off ~ x_continuous + x_binary + (1 | space_reg),
  f_pois_off_wi = y_pois_off ~ x_continuous + x_binary + offset(offset) + (1 | space_reg),
  f_qpois_off_wo = y_qpois_off ~ x_continuous + x_binary + (1 | space_reg),
  f_qpois_off_wi = y_qpois_off ~ x_continuous + x_binary + offset(offset) + (1 | space_reg)
)

test_that("merMod + f_lm", {
  if (!lme4_available) skip("lme4 not available")
  fit_mermod_lm <- lme4::lmer(formulas_mermod$f_lm,
                              data = data_mermod)
  expect_true("lmerMod" %in% class(fit_mermod_lm))
  check_mermod(ey_space, ey_time, fit_mermod_lm, data_mermod)
})

test_that("merMod + f_lm_noint", {
  if (!lme4_available) skip("lme4 not available")
  fit_mermod_lm_noint <- lme4::lmer(formulas_mermod$f_lm_noint,
                                    data = data_mermod)
  expect_true("lmerMod" %in% class(fit_mermod_lm_noint))
  check_mermod(ey_space, ey_time, fit_mermod_lm_noint, data_mermod)
})

test_that("merMod + f_lm_f", {
  if (!lme4_available) skip("lme4 not available")
  fit_mermod_lm_f <- lme4::lmer(formulas_mermod$f_lm_f,
                                    data = data_mermod)
  expect_true("lmerMod" %in% class(fit_mermod_lm_f))
  check_mermod(ey_space, ey_time, fit_mermod_lm_f, data_mermod)
})

test_that("merMod + f_logit (logit)", {
  if (!lme4_available) skip("lme4 not available")
  fit_mermod_glm_logit_lo <- lme4::glmer(formulas_mermod$f_logit,
                                         data = data_mermod,
                                         family = binomial(link = "logit"))
  expect_true("glmerMod" %in% class(fit_mermod_glm_logit_lo))
  check_mermod(ey_space, ey_time, fit_mermod_glm_logit_lo, data_mermod)
})

# Skipping the probit models because those gave me errors

test_that("merMod + f_logit_trans (logit)", {
  if (!lme4_available) skip("lme4 not available")
  fit_mermod_glm_logit_trans_lo <- lme4::glmer(formulas_mermod$f_logit_trans,
                                         data = data_mermod,
                                         family = binomial(link = "logit"))
  expect_true("glmerMod" %in% class(fit_mermod_glm_logit_trans_lo))
  check_mermod(ey_space, ey_time, fit_mermod_glm_logit_trans_lo, data_mermod)
})

test_that("merMod + f_logit_noint (logit)", {
  if (!lme4_available) skip("lme4 not available")
  fit_mermod_glm_logit_noint_lo <- lme4::glmer(formulas_mermod$f_logit_noint,
                                               data = data_mermod,
                                               family = binomial(link = "logit"))
  expect_true("glmerMod" %in% class(fit_mermod_glm_logit_noint_lo))
  check_mermod(ey_space, ey_time, fit_mermod_glm_logit_noint_lo, data_mermod)
})

test_that("merMod + f_logit_f (logit)", {
  if (!lme4_available) skip("lme4 not available")
  fit_mermod_glm_logit_f_lo <- lme4::glmer(formulas_mermod$f_logit_f,
                                               data = data_mermod,
                                               family = binomial(link = "logit"))
  expect_true("glmerMod" %in% class(fit_mermod_glm_logit_f_lo))
  check_mermod(ey_space, ey_time, fit_mermod_glm_logit_f_lo, data_mermod)
})

test_that("merMod + f_logit_noint_f (logit)", {
  if (!lme4_available) skip("lme4 not available")
  fit_mermod_glm_logit_noint_f_lo <- lme4::glmer(formulas_mermod$f_logit_noint_f,
                                           data = data_mermod,
                                           family = binomial(link = "logit"))
  expect_true("glmerMod" %in% class(fit_mermod_glm_logit_noint_f_lo))
  check_mermod(ey_space, ey_time, fit_mermod_glm_logit_noint_f_lo, data_mermod)
})

test_that("merMod + f_poiss", {
  if (!lme4_available) skip("lme4 not available")
  fit_mermod_glm_poiss <- lme4::glmer(formulas_mermod$f_pois,
                                                 data = data_mermod,
                                                 family = poisson(link = "log"))
  expect_true("glmerMod" %in% class(fit_mermod_glm_poiss))
  check_mermod(ey_space, ey_time, fit_mermod_glm_poiss, data_mermod)
})

test_that("merMod + f_poiss_f", {
  if (!lme4_available) skip("lme4 not available")
  fit_mermod_glm_poiss_f <- lme4::glmer(formulas_mermod$f_pois_f,
                                      data = data_mermod,
                                      family = poisson(link = "log"))
  expect_true("glmerMod" %in% class(fit_mermod_glm_poiss_f))
  check_mermod(ey_space, ey_time, fit_mermod_glm_poiss_f, data_mermod)
})

### f_pois_off ----
test_that("merMod + f_pois_off_wo / f_pois_off_wi", {
  if (!lme4_available) skip("lme4 not available")
  fit_mermod_glm_pois_off_wo_o <- lme4::glmer(formulas_mermod$f_pois_off_wo,
                                              data = data_mermod,
                                              family = poisson(link = "log"),
                                              offset = offset)
  expect_true("glmerMod" %in% class(fit_mermod_glm_pois_off_wo_o))
  expect_error(extract_yhat(ey_space, ey_time, fit_mermod_glm_pois_off_wo_o, data_mermod),
               "offset")

  coeffs_v1 <- suppressWarnings(report_model_coeff(fit_mermod_glm_pois_off_wo_o))
  expected_v1 <- predict(fit_mermod_glm_pois_off_wo_o, type = "response")

  fit_mermod_glm_pois_off_wi <- lme4::glmer(formulas_mermod$f_pois_off_wi,
                                     data = data_mermod,
                                     family = poisson(link = "log"))
  check_mermod(ey_space, ey_time, fit_mermod_glm_pois_off_wi, data_mermod)

  coeffs_v2 <- suppressWarnings(report_model_coeff(fit_mermod_glm_pois_off_wi))
  expected_v2 <- extract_yhat(ey_space, ey_time, fit_mermod_glm_pois_off_wi, data_mermod)

  expect_equivalent(as.list(coeffs_v1[, c(1:4)]),
                    as.list(coeffs_v2[, c(1:4)]),
                    tolerance = 0.05)
  expect_equivalent(expected_v1,
                    expected_v2[[3]][-na_inds],
                    tolerance = 0.01)
})

