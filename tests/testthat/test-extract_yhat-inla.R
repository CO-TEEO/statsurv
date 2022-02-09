# # THis has all been replaced by test-extract_yhat.R
# # Keeping it around to adapt into tests for tidy_inla and glance_inla
# inla_lookup <- tibble::tribble(~link_name, ~family, ~link,
#                                "gaussian", "gaussian", "identity",
#                                "logit", "binomial", "logit",
#                                "probit", "binomial", "probit",
#                                "poisson", "poisson", "log",
#                                "quasipoisson", "nbinomial", "log")
#
#
# inla_combos <- dplyr::left_join(glm_combos, inla_lookup, by = "link_name")
# inla_combos <- dplyr::bind_rows(dplyr::mutate(inla_combos, use_mer = TRUE),
#                          dplyr::mutate(inla_combos, use_mer = FALSE))
# for (ind in seq_len(nrow(inla_combos))) {
#
#   # if (ind %in% c(2, 8)) {
#   #   next
#   # }
#   print(ind)
#   curr_row <- inla_combos[ind, , drop = FALSE]
#   if (curr_row$use_mer) {
#      curr_f <- formulas_inla[[curr_row$f_name]]
#      curr_data <- ey_data_mermod
#      curr_newdata <- ey_newdata_mermod
#   } else {
#     curr_f <- formulas[[curr_row$f_name]]
#     curr_data <- ey_data
#     curr_newdata <- ey_newdata
#   }
#   if (curr_row$offset) {
#     fit <- INLA::inla(curr_f,
#                       data = curr_data,
#                       family = curr_row$family,
#                       control.family = list(link = curr_row$link),
#                       control.compute = list(config = TRUE),
#                       control.predictor = list(compute=TRUE),
#                       offset = offset)
#     yhat <- extract_yhat(fit, curr_newdata)
#
#     fit2 <- INLA::inla(curr_f,
#                        data = curr_data,
#                        family = curr_row$family,
#                        control.family = list(link = curr_row$link),
#                        control.compute = list(config = TRUE),
#                        control.predictor = list(compute=TRUE),
#                        E = exposure)
#     yhat2 <- extract_yhat(fit2, curr_newdata)
#     expect_equal(yhat, yhat2, tolerance = 0.01)
#   } else {
#     fit <- INLA::inla(curr_f,
#                       data = curr_data,
#                       family = curr_row$family,
#                       control.family = list(link = curr_row$link),
#                       control.compute = list(config = TRUE),
#                       control.predictor = list(compute=TRUE), verbose = FALSE)
#     yhat <- extract_yhat(fit, curr_newdata)
#   }
#
#   check_extract(fit, yhat, curr_f)
# }
#
# # inla_link_lookup <- list(gaussian =  "identity"),
# #                          logit = binomial(link = "logit"),
# #                          probit = binomial(link = "probit"),
# #                          poisson = poisson(link = "log"),
# #                          quasipoisson = quasipoisson(link = "log"))
#
#
#
# na_inds <- apply(ey_data, 1, function(x) {any(is.na(x))})
#
#
#
# test_that("INLA + f_lm", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_lm <- INLA::inla(formulas$f_lm,
#                             data = ey_data,
#                             family = "gaussian",
#                             control.compute = list(config = TRUE),
#                             control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_lm$summary.fitted.values$mean
#   extract_yhat(fit_inla_lm, ey_data)
#   check_inla(ey_space, ey_time, fit_inla_lm, ey_data, expected)
#
#   coeffs <- report_model_coeff(fit_inla_lm)
#   expect_equal(coeffs$term, c("(Intercept)", "x_continuous",
#                               "Precision for the Gaussian observations"))
#   expect_equal(coeffs$type, c("fixed", "fixed", "hyper"))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   coeffs2 <- report_model_coeff(fit_inla_lm, include_random = TRUE)
#   expect_equal(coeffs2[, -7], coeffs)
#   expect_true(TRUE)
# })
#
# test_that("INLA + f_lm_noint", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_lm_noint <- INLA::inla(formulas$f_lm_noint,
#                                   data = ey_data,
#                                   family = "gaussian",
#                                   control.compute = list(config = TRUE),
#                                   control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_lm_noint$summary.fitted.values$mean
#   check_inla(ey_space, ey_time, fit_inla_lm_noint, ey_data, expected)
#
#   coeffs <- report_model_coeff(fit_inla_lm_noint)
#   expect_equal(coeffs$term, c("x_continuous",
#                               "Precision for the Gaussian observations"))
#   expect_equal(coeffs$type, c("fixed", "hyper"))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#
#   expect_true(TRUE)
# })
#
# test_that("INLA + f_lm_f", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_lm_f <- INLA::inla(formulas$f_lm_f,
#                               data = ey_data,
#                               family = "gaussian",
#                               control.compute = list(config = TRUE),
#                               control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_lm_f$summary.fitted.values$mean
#   check_inla(ey_space, ey_time, fit_inla_lm_f, ey_data, expected)
#
#   coeffs <- report_model_coeff(fit_inla_lm_f)
#   expect_equal(coeffs$term, c("(Intercept)",
#                               "x_continuous", "x_discreteL2", "x_discreteL3",
#                               "x_discreteL4", "x_discreteL5",
#                               "Precision for the Gaussian observations"))
#   expect_equal(coeffs$type, c(rep("fixed", 6), "hyper"))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
# test_that("INLA + f_logit (logit)", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_logit_lo <- INLA::inla(formulas$f_logit,
#                                   data = ey_data,
#                                   family = "binomial",
#                                   control.family = list(link = "logit"),
#                                   Ntrials = rep(1, n),
#                                   control.compute = list(config = TRUE),
#                                   control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_logit_lo$summary.fitted.values$mean
#   expected[na_inds] <- arm::invlogit(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_logit_lo, ey_data, expected, tol = 0.01)
#
#   coeffs <- report_model_coeff(fit_inla_logit_lo)
#   expect_equal(coeffs$term, c("(Intercept)",
#                               "x_exp"))
#   expect_equal(coeffs$type, c(rep("fixed", 2), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
# test_that("INLA + f_logit (probit)", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_logit_pro <- INLA::inla(formulas$f_logit,
#                                data = ey_data,
#                                family = "binomial",
#                                control.family = list(link = "probit"),
#                                Ntrials = rep(1, n),
#                                control.compute = list(config = TRUE),
#                                control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_logit_pro$summary.fitted.values$mean
#   expected[na_inds] <- pnorm(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_logit_pro, ey_data, expected, tol = 0.01)
#   coeffs <- report_model_coeff(fit_inla_logit_pro)
#   expect_equal(coeffs$term, c("(Intercept)",
#                               "x_exp"))
#   expect_equal(coeffs$type, c(rep("fixed", 2), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
# test_that("INLA + f_logit_trans (logit)", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_logit_trans_lo <- INLA::inla(formulas$f_logit_trans,
#                                         data = ey_data,
#                                         family = "binomial",
#                                         control.family = list(link = "logit"),
#                                         Ntrials = rep(1, n),
#                                         control.compute = list(config = TRUE),
#                                         control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_logit_trans_lo$summary.fitted.values$mean
#   expected[na_inds] <- arm::invlogit(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_logit_trans_lo, ey_data, expected, tol = 0.01)
#
#   coeffs <- report_model_coeff(fit_inla_logit_trans_lo)
#   expect_equal(coeffs$term, c("(Intercept)",
#                               "log(x_exp)"))
#   expect_equal(coeffs$type, c(rep("fixed", 2), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
# test_that("INLA + f_logit_trans (probit)", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_logit_trans_pro <- INLA::inla(formulas$f_logit_trans,
#                                         data = ey_data,
#                                         family = "binomial",
#                                         control.family = list(link = "probit"),
#                                         Ntrials = rep(1, n),
#                                         control.compute = list(config = TRUE),
#                                         control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_logit_trans_pro$summary.fitted.values$mean
#   expected[na_inds] <- pnorm(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_logit_trans_pro, ey_data, expected, tol = 0.01)
#   coeffs <- report_model_coeff(fit_inla_logit_trans_pro)
#   expect_equal(coeffs$term, c("(Intercept)",
#                               "log(x_exp)"))
#   expect_equal(coeffs$type, c(rep("fixed", 2), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
#
# ### f_logit_noint ----
# test_that("INLA + f_logit_noint (logit)", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_logit_noint_lo <- INLA::inla(formulas$f_logit_noint,
#                                         data = ey_data,
#                                         family = "binomial",
#                                         control.family = list(link = "logit"),
#                                         Ntrials = rep(1, n),
#                                         control.compute = list(config = TRUE),
#                                         control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_logit_noint_lo$summary.fitted.values$mean
#   expected[na_inds] <- arm::invlogit(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_logit_noint_lo, ey_data, expected, tol = 0.01)
#   expect_true(TRUE)
#   coeffs <- report_model_coeff(fit_inla_logit_noint_lo)
#   expect_equal(coeffs$term, c("x_exp"))
#   expect_equal(coeffs$type, c(rep("fixed", 1), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
# test_that("INLA + f_logit_noint (probit)", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_logit_noint_pro <- INLA::inla(formulas$f_logit_noint,
#                                          data = ey_data,
#                                          family = "binomial",
#                                          control.family = list(link = "probit"),
#                                          Ntrials = rep(1, n),
#                                          control.compute = list(config = TRUE),
#                                          control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_logit_noint_pro$summary.fitted.values$mean
#   expected[na_inds] <- pnorm(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_logit_noint_pro, ey_data, expected, tol = 0.01)
#   coeffs <- report_model_coeff(fit_inla_logit_noint_pro)
#   expect_equal(coeffs$term, c("x_exp"))
#   expect_equal(coeffs$type, c(rep("fixed", 1), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
# ### f_logit_f ----
# test_that("INLA + f_logit_f (logit)", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_logit_f_lo <- INLA::inla(formulas$f_logit_f,
#                                         data = ey_data,
#                                         family = "binomial",
#                                         control.family = list(link = "logit"),
#                                         Ntrials = rep(1, n),
#                                         control.compute = list(config = TRUE),
#                                         control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_logit_f_lo$summary.fitted.values$mean
#   expected[na_inds] <- arm::invlogit(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_logit_f_lo, ey_data, expected, tol = 0.01)
#   coeffs <- report_model_coeff(fit_inla_logit_f_lo)
#   expect_equal(coeffs$term, c("(Intercept)",
#                               "x_exp", "x_discreteL2", "x_discreteL3",
#                               "x_discreteL4", "x_discreteL5"))
#   expect_equal(coeffs$type, c(rep("fixed", 6), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
# test_that("INLA + f_logit_f (probit)", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_logit_f_pro <- INLA::inla(formulas$f_logit_f,
#                                          data = ey_data,
#                                          family = "binomial",
#                                          control.family = list(link = "probit"),
#                                          Ntrials = rep(1, n),
#                                          control.compute = list(config = TRUE),
#                                          control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_logit_f_pro$summary.fitted.values$mean
#   expected[na_inds] <- pnorm(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_logit_f_pro, ey_data, expected, tol = 0.01)
#   coeffs <- report_model_coeff(fit_inla_logit_f_pro)
#   expect_equal(coeffs$term, c("(Intercept)",
#                               "x_exp", "x_discreteL2", "x_discreteL3",
#                               "x_discreteL4", "x_discreteL5"))
#   expect_equal(coeffs$type, c(rep("fixed", 6), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
#
# ### f_logit_noint_f ----
# test_that("INLA + f_logit_nonit_f (logit)", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_logit_noint_f_lo <- INLA::inla(formulas$f_logit_noint_f,
#                                     data = ey_data,
#                                     family = "binomial",
#                                     control.family = list(link = "logit"),
#                                     Ntrials = rep(1, n),
#                                     control.compute = list(config = TRUE),
#                                     control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_logit_noint_f_lo$summary.fitted.values$mean
#   expected[na_inds] <- arm::invlogit(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_logit_noint_f_lo, ey_data, expected, tol = 0.01)
#   coeffs <- report_model_coeff(fit_inla_logit_noint_f_lo)
#   expect_equal(coeffs$term, c("x_exp", "x_discreteL1",
#                               "x_discreteL2", "x_discreteL3",
#                               "x_discreteL4", "x_discreteL5"))
#   expect_equal(coeffs$type, c(rep("fixed", 6), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
# test_that("INLA + f_logit_nonit_f (probit)", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_logit_noint_f_pro <- INLA::inla(formulas$f_logit_noint_f,
#                                      data = ey_data,
#                                      family = "binomial",
#                                      control.family = list(link = "probit"),
#                                      Ntrials = rep(1, n),
#                                      control.compute = list(config = TRUE),
#                                      control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_logit_noint_f_pro$summary.fitted.values$mean
#   expected[na_inds] <- pnorm(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_logit_noint_f_pro, ey_data, expected, tol = 0.01)
#   coeffs <- report_model_coeff(fit_inla_logit_noint_f_pro)
#   expect_equal(coeffs$term, c("x_exp", "x_discreteL1",
#                               "x_discreteL2", "x_discreteL3",
#                               "x_discreteL4", "x_discreteL5"))
#   expect_equal(coeffs$type, c(rep("fixed", 6), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
#
# ### f_pois ----
# test_that("INLA + f_pois", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_pois <- INLA::inla(formulas$f_pois,
#                               data = ey_data,
#                               family = "poisson",
#                               control.family = list(link = "log"),
#                               control.compute = list(config = TRUE),
#                               control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_pois$summary.fitted.values$mean
#   expected[na_inds] <- exp(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_pois, ey_data, expected, tol = 0.01)
#   coeffs <- report_model_coeff(fit_inla_pois)
#   expect_equal(coeffs$term, c("(Intercept)", "x_continuous", "x_binary"))
#   expect_equal(coeffs$type, c(rep("fixed", 3), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
# ### f_pois_f ----
# test_that("INLA + f_pois_f", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_pois_f <- INLA::inla(formulas$f_pois_f,
#                                 data = ey_data,
#                                 family = "poisson",
#                                 control.family = list(link = "log"),
#                                 control.compute = list(config = TRUE),
#                                 control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_pois_f$summary.fitted.values$mean
#   expected[na_inds] <- exp(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_pois_f, ey_data, expected, tol = 0.01)
#   coeffs <- report_model_coeff(fit_inla_pois_f)
#   expect_equal(coeffs$term, c("(Intercept)", "x_continuous", "factor(x_binary)1"))
#   expect_equal(coeffs$type, c(rep("fixed", 3), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
# })
#
# ### f_pois_off ----
# test_that("INLA + f_pois_off_wo / f_pois_off_wi", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_pois_off_wo_o <- INLA::inla(formulas$f_pois_off_wo,
#                                        data = ey_data,
#                                        family = "poisson",
#                                        control.family = list(link = "log"),
#                                        control.compute = list(config = TRUE),
#                                        control.predictor = list(compute=TRUE),
#                                        offset = offset)
#
#   expected <- fit_inla_pois_off_wo_o$summary.fitted.values$mean
#   expected[na_inds] <- exp(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_pois_off_wo_o, ey_data, expected, tol = 0.03)
#   coeffs <- report_model_coeff(fit_inla_pois_off_wo_o)
#   expect_equal(coeffs$term, c("(Intercept)", "x_continuous", "x_binary"))
#   expect_equal(coeffs$type, c(rep("fixed", 3), rep("hyper", 0)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#   expect_true(TRUE)
#
#   coeffs_v1 <- coeffs
#   expected_v1 <- expected
#
#   fit_glm_pois_off_wo_offset <- glm(formulas$f_pois_off_wo,
#                                     data = ey_data,
#                                     offset = offset,
#                                     family = poisson(link = "log"))
#   expect_equal(expected_v1,
#                extract_yhat(ey_space, ey_time, fit_glm_pois_off_wo_offset, ey_data)[[3]],
#                tolerance = 0.01)
#
#   expect_equivalent(as.list(coeffs_v1[, c(-5, -6)]),
#                     as.list(report_model_coeff(fit_glm_pois_off_wo_offset)[, c(-5)]),
#                     tolerance = 0.05)
#
#   fit_inla_pois_off_wo_e <- INLA::inla(formulas$f_pois_off_wo,
#                                        data = ey_data,
#                                        family = "poisson",
#                                        control.family = list(link = "log"),
#                                        control.compute = list(config = TRUE),
#                                        control.predictor = list(compute=TRUE),
#                                        E = exposure)
#
#   expected <- fit_inla_pois_off_wo_e$summary.fitted.values$mean
#   expected[na_inds] <- exp(expected[na_inds])
#   expected <- expected * exposure
#   expected_v2 <- expected
#
#
#   check_inla(ey_space, ey_time, fit_inla_pois_off_wo_e, ey_data, expected, tol = 0.01)
#   expect_equivalent(as.list(coeffs_v1),
#                     as.list(report_model_coeff(fit_inla_pois_off_wo_e)),
#                     tolerance = 0.05)
#
#
#   fit_inla_pois_off_wi <- INLA::inla(formulas$f_pois_off_wi,
#                                        data = ey_data,
#                                        family = "poisson",
#                                        control.family = list(link = "log"),
#                                        control.compute = list(config = TRUE),
#                                        control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_pois_off_wi$summary.fitted.values$mean
#   expected[na_inds] <- exp(expected[na_inds])
#   expected_v3 <- expected
#   check_inla(ey_space, ey_time, fit_inla_pois_off_wi, ey_data, expected, tol = 0.01)
#   expect_equivalent(as.list(coeffs_v1),
#                     as.list(report_model_coeff(fit_inla_pois_off_wi)),
#                     tolerance = 0.05)
#
#
#   expect_equal(expected_v2, expected_v1)
#   expect_equal(expected_v3, expected_v1)
# })
#
# ### f_qpois_off ----
# test_that("INLA + f_qpois_off_wo / f_qpois_off_wi", {
#   if (!inla_available) skip("INLA not available")
#   fit_inla_qpois_off_wo_o <- INLA::inla(formulas$f_qpois_off_wo,
#                                        data = ey_data,
#                                        family = "nbinomial",
#                                        control.family = list(link = "log"),
#                                        control.compute = list(config = TRUE),
#                                        control.predictor = list(compute=TRUE),
#                                        offset = offset)
#
#   expected <- fit_inla_qpois_off_wo_o$summary.fitted.values$mean
#   expected[na_inds] <- exp(expected[na_inds])
#   check_inla(ey_space, ey_time, fit_inla_qpois_off_wo_o, ey_data, expected,
#              tol = 0.1, n_samples = 5000)
#   coeffs <- report_model_coeff(fit_inla_qpois_off_wo_o)
#   expect_equal(coeffs$term, c("(Intercept)", "x_continuous", "x_binary",
#                               "size for the nbinomial observations (1/overdispersion)"))
#   expect_equal(coeffs$type, c(rep("fixed", 3), rep("hyper", 1)))
#   expect_true(tibble::is_tibble(coeffs))
#   expect_true(is.numeric(coeffs$estimate))
#
#   coeffs_v1 <- coeffs
#   expected_v1 <- expected
#
#   fit_inla_qpois_off_wo_e <- INLA::inla(formulas$f_qpois_off_wo,
#                                        data = ey_data,
#                                        family = "nbinomial",
#                                        control.family = list(link = "log"),
#                                        control.compute = list(config = TRUE),
#                                        control.predictor = list(compute=TRUE),
#                                        E = exposure)
#
#   expected <- fit_inla_qpois_off_wo_e$summary.fitted.values$mean
#   expected[na_inds] <- exp(expected[na_inds])
#   expected <- expected * exposure
#   expected_v2 <- expected
#
#   check_inla(ey_space, ey_time, fit_inla_qpois_off_wo_e, ey_data, expected,
#              tol = 0.1, n_samples = 5000)
#   expect_equivalent(as.list(coeffs_v1),
#                     as.list(report_model_coeff(fit_inla_qpois_off_wo_e)),
#                     tolerance = 0.05)
#
#   fit_inla_qpois_off_wi <- INLA::inla(formulas$f_qpois_off_wi,
#                                      data = ey_data,
#                                      family = "nbinomial",
#                                      control.family = list(link = "log"),
#                                      control.compute = list(config = TRUE),
#                                      control.predictor = list(compute=TRUE))
#
#   expected <- fit_inla_qpois_off_wi$summary.fitted.values$mean
#   expected[na_inds] <- exp(expected[na_inds])
#   expected_v3 <- expected
#   check_inla(ey_space, ey_time, fit_inla_qpois_off_wi, ey_data, expected,
#              tol = 0.1, n_samples = 5000)
#   expect_equivalent(as.list(coeffs_v1),
#                     as.list(report_model_coeff(fit_inla_qpois_off_wi)),
#                     tolerance = 0.05)
#
#   expect_equal(expected_v2, expected_v1, tolerance = 0.01)
#   expect_equal(expected_v3, expected_v1, tolerance = 0.01)
# })
#
#
#
#
#
#
