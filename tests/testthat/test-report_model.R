# Most of the tests for report_model_coefficients are in test-extract_yhat-inla.R,
# But some of them didn't fit well there and so they're here instead.

# Two goals:
# 1. Testing multi-level models.
# 2. Testing the report_model_summary

#
# fit_inla_lm <- INLA::inla(formulas$f_lm,
#                           data = ey_data,
#                           family = "gaussian",
#                           control.compute = list(config = TRUE),
#                           control.predictor = list(compute=TRUE))
#
test_that("Works on multilevel models", {
  skip_if_not(inla_available)
  formula_varint <- y_varint ~ 1 + x_continuous + f(space_reg, model = "iid")
  fit_inla_varint <- INLA::inla(formula = formula_varint,
                                family = "gaussian",
                                data = ey_data,
                                control.compute = list(config = TRUE),
                                control.predictor = list(compute=TRUE))

  coeffs_norandom <- report_model_coeff(fit_inla_varint)
  expect_equal(coeffs_norandom$term, c("(Intercept)", "x_continuous",
                                       "Precision for the Gaussian observations",
                                       "Precision for space_reg"))
  expect_equal(coeffs_norandom$type,
               c(rep("fixed", 2), rep("hyper", 2)))
  expect_true(tibble::is_tibble(coeffs_norandom))
  expect_true(is.numeric(coeffs_norandom$estimate))

  coeffs_yesrandom <- report_model_coeff(fit_inla_varint, TRUE)
  expect_equal(coeffs_yesrandom$term, c("(Intercept)", "x_continuous",
                                        "Precision for the Gaussian observations",
                                        "Precision for space_reg",
                                        gridcoord::gc_get_labels(ey_space)))
  expect_equal(coeffs_yesrandom$type,
               c(rep("fixed", 2), rep("hyper", 2), rep("random", nrow(ey_space))))
  expect_equal(coeffs_yesrandom$group,
               c(rep(NA, 4), rep("space_reg", nrow(ey_space))))

})

test_that("Works on multilevel models with multiple random variables", {
  skip_if_not(inla_available)
  space_slopes <- ey_space %>%
    dplyr::rowwise() %>%
    dplyr::mutate(space_slopes = rnorm(sd = 2,n = 1)) %>%
    .$space_slopes %>%
    magrittr::set_names(gridcoord::gc_get_labels(space_coord))

  df <- ey_data
  df$y_varvar <- 2 + df$x_continuous * space_slopes[df$space_reg] +
    space_coeffs[df$space_reg] + rnorm(n, sd = 2)
  df$space_reg2 <- df$space_reg

  formula_varvar <- y_varvar ~ 1 + f(space_reg, x_continuous) +
    f(space_reg2, model = "iid")
  fit_inla_varvar <- INLA::inla(formula = formula_varvar,
                                family = "gaussian",
                                data = df,
                                control.compute = list(config = TRUE),
                                control.predictor = list(compute=TRUE))

  coeffs_yesrandom <- report_model_coeff(fit_inla_varvar, TRUE)
  expect_equal(coeffs_yesrandom$term, c("(Intercept)",
                                        "Precision for the Gaussian observations",
                                        "Precision for space_reg",
                                        "Precision for space_reg2",
                                        gridcoord::gc_get_labels(ey_space),
                                        gridcoord::gc_get_labels(ey_space)))
  expect_equal(coeffs_yesrandom$type,
               c(rep("fixed", 1), rep("hyper", 3), rep("random", 2*nrow(ey_space))))
  expect_equal(coeffs_yesrandom$group,
               c(rep(NA, 4),
                 rep("space_reg", nrow(ey_space)),
                 rep("space_reg2", nrow(ey_space))))

  res <- report_model_summary(fit_inla_varvar)
  expect_equal(nrow(res), 1)
})

test_that("report_model_summary gives out numbers", {
  fit1 <- INLA::inla(formulas$f_lm,
                            data = ey_data,
                            family = "gaussian",
                            control.compute = list(config = TRUE),
                            control.predictor = list(compute=TRUE))

  summary1 <- as.data.frame(report_model_summary(fit1))
  expect_equal(is.na(summary1)[1, ],
               c(cpu.time = F, deviance = T, logLik = F, DIC = T, DIC.sat = T,
                 WAIC = T, cpo.failures = F, cpo.logsum = F, n.eff.param = F))
  expect_equivalent(summary1$n.eff.param, 2, tolerance = 1e-3)
  expect_equivalent(summary1$cpo.logsum, 0, tolerance = 1e-3)

  fit2 <- INLA::inla(formulas$f_lm,
                     data = ey_data,
                     family = "gaussian",
                     control.compute = list(config = TRUE, dic = TRUE),
                     control.predictor = list(compute=TRUE))
  summary2 <- as.data.frame(report_model_summary(fit2))
  expect_equal(is.na(summary2)[1, ],
               c(cpu.time = F, deviance = F, logLik = F, DIC = F, DIC.sat = F,
                 WAIC = T, cpo.failures = F, cpo.logsum = F, n.eff.param = F))
  expect_equivalent(summary2$n.eff.param, 2, tolerance = 1e-3)
  expect_equivalent(summary2$cpo.logsum, 0, tolerance = 1e-3)

  fit3 <- INLA::inla(formulas$f_lm,
                     data = ey_data,
                     family = "gaussian",
                     control.compute = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE),
                     control.predictor = list(compute=TRUE))
  summary3 <- as.data.frame(report_model_summary(fit3))
  expect_equal(is.na(summary3)[1, ],
               c(cpu.time = F, deviance = F, logLik = F, DIC = F, DIC.sat = F,
                 WAIC = F, cpo.failures = F, cpo.logsum = F, n.eff.param = F))
  expect_equivalent(summary2$n.eff.param, 2, tolerance = 1e-3)
  expect_false(summary3$cpo.logsum == 0)
})
