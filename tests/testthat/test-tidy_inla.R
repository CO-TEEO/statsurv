test_that("Works on multilevel models", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  formula_varint <- y_varint ~ 1 + x_continuous + f(id_space, model = "iid")
  fit_inla_varint <- INLA::inla(formula = formula_varint,
                                family = "gaussian",
                                data = ey_data,
                                control.compute = list(config = TRUE),
                                control.predictor = list(compute=TRUE))

  coeffs_norandom <- tidy_inla(fit_inla_varint)
  expect_equal(coeffs_norandom$term, c("(Intercept)", "x_continuous",
                                       "Precision for the Gaussian observations",
                                       "Precision for id_space"))
  expect_equal(coeffs_norandom$type,
               c(rep("fixed", 2), rep("hyper", 2)))
  expect_true(tibble::is_tibble(coeffs_norandom))
  expect_true(is.numeric(coeffs_norandom$estimate))

  coeffs_yesrandom <- tidy_inla(fit_inla_varint, TRUE)
  expect_equal(coeffs_yesrandom$term, c("(Intercept)", "x_continuous",
                                        "Precision for the Gaussian observations",
                                        "Precision for id_space",
                                        sort(unique(ey_data$id_space))))
  expect_equal(coeffs_yesrandom$type,
               c(rep("fixed", 2), rep("hyper", 2), rep("random", max(ey_data$id_space))))
  expect_equal(coeffs_yesrandom$group,
               c(rep(NA, 4), rep("id_space", max(ey_data$id_space))))

})

test_that("Works on multilevel models with multiple random variables", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  space_slopes <- rnorm(n = max(ey_data$id_space), sd = 2)

  df <- ey_data
  df$y_varvar <- 2 + df$x_continuous * space_slopes[df$id_space] +
    space_coeffs[df$id_space] + rnorm(n, sd = 2)
  df$id_space2 <- df$id_space

  formula_varvar <- y_varvar ~ 1 + f(id_space, x_continuous) +
    f(id_space2, model = "iid")
  fit_inla_varvar <- INLA::inla(formula = formula_varvar,
                                family = "gaussian",
                                data = df,
                                control.compute = list(config = TRUE),
                                control.predictor = list(compute=TRUE))

  coeffs_yesrandom <- tidy_inla(fit_inla_varvar, TRUE)
  expect_equal(coeffs_yesrandom$term, c("(Intercept)",
                                        "Precision for the Gaussian observations",
                                        "Precision for id_space",
                                        "Precision for id_space2",
                                        sort(unique(ey_data$id_space)),
                                        sort(unique(ey_data$id_space))))
  expect_equal(coeffs_yesrandom$type,
               c(rep("fixed", 1), rep("hyper", 3), rep("random", 2*max(ey_data$id_space))))
  expect_equal(coeffs_yesrandom$group,
               c(rep(NA, 4),
                 rep("id_space", max(ey_data$id_space)),
                 rep("id_space2", max(ey_data$id_space))))

  res <- glance_inla(fit_inla_varvar)
  expect_equal(nrow(res), 1)
})

test_that("glance_inla gives out numbers", {
  skip_on_cran()
  skip_if_not_installed("INLA")


  fit1 <- INLA::inla(formulas$f_lm,
                            data = ey_data,
                            family = "gaussian",
                            control.compute = list(config = TRUE),
                            control.predictor = list(compute=TRUE))

  summary1 <- as.data.frame(glance_inla(fit1))
  expect_equal(is.na(summary1)[1, ],
               c(cpu.time = F, deviance = T, logLik = F, DIC = T, DIC.sat = T,
                 WAIC = T, cpo.failures = F, cpo.logsum = F))
  expect_equivalent(summary1$cpo.logsum, 0, tolerance = 1e-3)

  fit2 <- INLA::inla(formulas$f_lm,
                     data = ey_data,
                     family = "gaussian",
                     control.compute = list(config = TRUE, dic = TRUE),
                     control.predictor = list(compute=TRUE))
  summary2 <- as.data.frame(glance_inla(fit2))
  expect_equal(is.na(summary2)[1, ],
               c(cpu.time = F, deviance = F, logLik = F, DIC = F, DIC.sat = F,
                 WAIC = T, cpo.failures = F, cpo.logsum = F))
  expect_equivalent(summary2$cpo.logsum, 0, tolerance = 1e-3)

  fit3 <- INLA::inla(formulas$f_lm,
                     data = ey_data,
                     family = "gaussian",
                     control.compute = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE),
                     control.predictor = list(compute=TRUE))
  summary3 <- as.data.frame(glance_inla(fit3))
  expect_equal(is.na(summary3)[1, ],
               c(cpu.time = F, deviance = F, logLik = F, DIC = F, DIC.sat = F,
                 WAIC = F, cpo.failures = F, cpo.logsum = F))
  expect_false(summary3$cpo.logsum == 0)
})

test_that("INLA + f_lm", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  fit_inla_lm <- INLA::inla(formulas$f_lm,
                            data = ey_data,
                            family = "gaussian",
                            control.compute = list(config = TRUE),
                            control.predictor = list(compute=TRUE))

  coeffs <- tidy_inla(fit_inla_lm)
  expect_equal(coeffs$term, c("(Intercept)", "x_continuous",
                              "Precision for the Gaussian observations"))
  expect_equal(coeffs$type, c("fixed", "fixed", "hyper"))
  expect_true(tibble::is_tibble(coeffs))
  expect_true(is.numeric(coeffs$estimate))
  coeffs2 <- tidy_inla(fit_inla_lm, include_random = TRUE)
  expect_equal(coeffs2[, -7], coeffs)
})

test_that("INLA + f_lm_noint", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  fit_inla_lm_noint <- INLA::inla(formulas$f_lm_noint,
                                  data = ey_data,
                                  family = "gaussian",
                                  control.compute = list(config = TRUE),
                                  control.predictor = list(compute=TRUE))

  coeffs <- tidy_inla(fit_inla_lm_noint)
  expect_equal(coeffs$term, c("x_continuous",
                              "Precision for the Gaussian observations"))
  expect_equal(coeffs$type, c("fixed", "hyper"))
  expect_true(tibble::is_tibble(coeffs))
  expect_true(is.numeric(coeffs$estimate))
})

test_that("INLA + f_lm_f", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  fit_inla_lm_f <- INLA::inla(formulas$f_lm_f,
                              data = ey_data,
                              family = "gaussian",
                              control.compute = list(config = TRUE),
                              control.predictor = list(compute=TRUE))

  coeffs <- tidy_inla(fit_inla_lm_f)
  expect_equal(coeffs$term, c("(Intercept)",
                              "x_continuous", "x_discreteL2", "x_discreteL3",
                              "x_discreteL4", "x_discreteL5",
                              "Precision for the Gaussian observations"))
  expect_equal(coeffs$type, c(rep("fixed", 6), "hyper"))
  expect_true(tibble::is_tibble(coeffs))
  expect_true(is.numeric(coeffs$estimate))
})

test_that("INLA + f_logit (logit)", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  fit_inla_logit_lo <- INLA::inla(formulas$f_logit,
                                  data = ey_data,
                                  family = "binomial",
                                  control.family = list(link = "logit"),
                                  Ntrials = rep(1, n),
                                  control.compute = list(config = TRUE),
                                  control.predictor = list(compute=TRUE))

  coeffs <- tidy_inla(fit_inla_logit_lo)
  expect_equal(coeffs$term, c("(Intercept)",
                              "x_exp"))
  expect_equal(coeffs$type, c(rep("fixed", 2), rep("hyper", 0)))
  expect_true(tibble::is_tibble(coeffs))
  expect_true(is.numeric(coeffs$estimate))
  expect_true(TRUE)
})


test_that("INLA + f_logit_trans (logit)", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  fit_inla_logit_trans_lo <- INLA::inla(formulas$f_logit_trans,
                                        data = ey_data,
                                        family = "binomial",
                                        control.family = list(link = "logit"),
                                        Ntrials = rep(1, n),
                                        control.compute = list(config = TRUE),
                                        control.predictor = list(compute=TRUE))

  coeffs <- tidy_inla(fit_inla_logit_trans_lo)
  expect_equal(coeffs$term, c("(Intercept)",
                              "log(x_exp)"))
  expect_equal(coeffs$type, c(rep("fixed", 2), rep("hyper", 0)))
  expect_true(tibble::is_tibble(coeffs))
  expect_true(is.numeric(coeffs$estimate))
})

test_that("INLA + f_logit_trans (probit)", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  fit_inla_logit_trans_pro <- INLA::inla(formulas$f_logit_trans,
                                         data = ey_data,
                                         family = "binomial",
                                         control.family = list(link = "probit"),
                                         Ntrials = rep(1, n),
                                         control.compute = list(config = TRUE),
                                         control.predictor = list(compute=TRUE))


  coeffs <- tidy_inla(fit_inla_logit_trans_pro)
  expect_equal(coeffs$term, c("(Intercept)",
                              "log(x_exp)"))
  expect_equal(coeffs$type, c(rep("fixed", 2), rep("hyper", 0)))
  expect_true(tibble::is_tibble(coeffs))
  expect_true(is.numeric(coeffs$estimate))
  expect_true(TRUE)
})


### f_pois ----
test_that("INLA + f_pois", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  fit_inla_pois <- INLA::inla(formulas$f_pois,
                              data = ey_data,
                              family = "poisson",
                              control.family = list(link = "log"),
                              control.compute = list(config = TRUE),
                              control.predictor = list(compute=TRUE))


  coeffs <- tidy_inla(fit_inla_pois)
  expect_equal(coeffs$term, c("(Intercept)", "x_continuous", "x_binary"))
  expect_equal(coeffs$type, c(rep("fixed", 3), rep("hyper", 0)))
  expect_true(tibble::is_tibble(coeffs))
  expect_true(is.numeric(coeffs$estimate))
  expect_true(TRUE)
})

### f_pois_f ----
test_that("INLA + f_pois_f", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  fit_inla_pois_f <- INLA::inla(formulas$f_pois_f,
                                data = ey_data,
                                family = "poisson",
                                control.family = list(link = "log"),
                                control.compute = list(config = TRUE),
                                control.predictor = list(compute=TRUE))

  coeffs <- tidy_inla(fit_inla_pois_f)
  expect_equal(coeffs$term, c("(Intercept)", "x_continuous", "factor(x_binary)1"))
  expect_equal(coeffs$type, c(rep("fixed", 3), rep("hyper", 0)))
  expect_true(tibble::is_tibble(coeffs))
  expect_true(is.numeric(coeffs$estimate))
  expect_true(TRUE)
})

### f_pois_off ----
test_that("INLA + f_pois_off_wo / f_pois_off_wi", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  fit_inla_pois_off_wo_o <- INLA::inla(formulas$f_pois_off_wo,
                                       data = ey_data,
                                       family = "poisson",
                                       control.family = list(link = "log"),
                                       control.compute = list(config = TRUE),
                                       control.predictor = list(compute=TRUE),
                                       offset = offset)

  coeffs <- tidy_inla(fit_inla_pois_off_wo_o)
  expect_equal(coeffs$term, c("(Intercept)", "x_continuous", "x_binary"))
  expect_equal(coeffs$type, c(rep("fixed", 3), rep("hyper", 0)))
  expect_true(tibble::is_tibble(coeffs))
  expect_true(is.numeric(coeffs$estimate))
  expect_true(TRUE)

  coeffs_v1 <- coeffs

  fit_glm_pois_off_wo_offset <- glm(formulas$f_pois_off_wo,
                                    data = ey_data,
                                    offset = offset,
                                    family = poisson(link = "log"))

  expect_equivalent(as.list(coeffs_v1[, c(-5, -6)]),
                    as.list(broom::tidy(fit_glm_pois_off_wo_offset)[, c(-5)]),
                    tolerance = 0.05)

  fit_inla_pois_off_wo_e <- INLA::inla(formulas$f_pois_off_wo,
                                       data = ey_data,
                                       family = "poisson",
                                       control.family = list(link = "log"),
                                       control.compute = list(config = TRUE),
                                       control.predictor = list(compute=TRUE),
                                       E = exposure)


  expect_equivalent(as.list(coeffs_v1),
                    as.list(tidy_inla(fit_inla_pois_off_wo_e)),
                    tolerance = 0.05)


  fit_inla_pois_off_wi <- INLA::inla(formulas$f_pois_off_wi,
                                     data = ey_data,
                                     family = "poisson",
                                     control.family = list(link = "log"),
                                     control.compute = list(config = TRUE),
                                     control.predictor = list(compute=TRUE))

  expect_equivalent(as.list(coeffs_v1),
                    as.list(tidy_inla(fit_inla_pois_off_wi)),
                    tolerance = 0.05)
})

### f_qpois_off ----
test_that("INLA + f_qpois_off_wo / f_qpois_off_wi", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  fit_inla_qpois_off_wo_o <- INLA::inla(formulas$f_qpois_off_wo,
                                        data = ey_data,
                                        family = "nbinomial",
                                        control.family = list(link = "log"),
                                        control.compute = list(config = TRUE),
                                        control.predictor = list(compute=TRUE),
                                        offset = offset)

  coeffs <- tidy_inla(fit_inla_qpois_off_wo_o)
  expect_equal(coeffs$term, c("(Intercept)", "x_continuous", "x_binary",
                              "size for the nbinomial observations (1/overdispersion)"))
  expect_equal(coeffs$type, c(rep("fixed", 3), rep("hyper", 1)))
  expect_true(tibble::is_tibble(coeffs))
  expect_true(is.numeric(coeffs$estimate))

  coeffs_v1 <- coeffs

  fit_inla_qpois_off_wo_e <- INLA::inla(formulas$f_qpois_off_wo,
                                        data = ey_data,
                                        family = "nbinomial",
                                        control.family = list(link = "log"),
                                        control.compute = list(config = TRUE),
                                        control.predictor = list(compute=TRUE),
                                        E = exposure)

  expect_equivalent(as.list(coeffs_v1),
                    as.list(tidy_inla(fit_inla_qpois_off_wo_e)),
                    tolerance = 0.05)

  fit_inla_qpois_off_wi <- INLA::inla(formulas$f_qpois_off_wi,
                                      data = ey_data,
                                      family = "nbinomial",
                                      control.family = list(link = "log"),
                                      control.compute = list(config = TRUE),
                                      control.predictor = list(compute=TRUE))

  expect_equivalent(as.list(coeffs_v1),
                    as.list(tidy_inla(fit_inla_qpois_off_wi)),
                    tolerance = 0.05)
})







