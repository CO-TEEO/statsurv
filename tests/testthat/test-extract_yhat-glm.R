### f_lm ----
fit_glm_lm <- glm(formulas$f_lm,
                  data = ey_data,
                  family = gaussian(link = "identity"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_lm))
})
check_yhat_means(ey_space, ey_time, fit_glm_lm, ey_data)

### f_lm_noint ----
fit_glm_lm_noint <- glm(formulas$f_lm_noint,
                        data = ey_data,
                        family = gaussian(link = "identity"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_lm_noint))
})
check_yhat_means(ey_space, ey_time, fit_glm_lm_noint, ey_data)

### f_lm_f ----
fit_glm_lm_f <- glm(formulas$f_lm_f,
                data = ey_data,
                family = gaussian(link = "identity"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_lm_f))
})
check_yhat_means(ey_space, ey_time, fit_glm_lm_f, ey_data)


### f_logit (logit) ----
fit_glm_logit_lo <- glm(formulas$f_logit,
                 data = ey_data,
                 family = binomial(link = "logit"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_logit_lo))
})
check_yhat_means(ey_space, ey_time, fit_glm_logit_lo, ey_data)

### f_logit (probit) ----
fit_glm_logit_pro <- glm(formulas$f_logit,
                         data = ey_data,
                         family = binomial(link = "probit"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_logit_pro))
})
check_yhat_means(ey_space, ey_time, fit_glm_logit_pro, ey_data)

### f_logit_trans (logit) ----
fit_glm_logit_trans_lo <- glm(formulas$f_logit_trans,
                              data = ey_data,
                              family = binomial(link = "logit"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_logit_trans_lo))
})
check_yhat_means(ey_space, ey_time, fit_glm_logit_trans_lo, ey_data)

### f_logit_trans (probit) ----
fit_glm_logit_trans_pro <- glm(formulas$f_logit_trans,
                              data = ey_data,
                              family = binomial(link = "probit"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_logit_trans_pro))
})
check_yhat_means(ey_space, ey_time, fit_glm_logit_trans_pro, ey_data)

### f_logit_noint (logit) ----
fit_glm_logit_noint_lo <- glm(formulas$f_logit_noint,
                              data = ey_data,
                              family = binomial(link = "logit"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_logit_noint_lo))
})
check_yhat_means(ey_space, ey_time, fit_glm_logit_noint_lo, ey_data)

### f_logit_noint (probit) ----
fit_glm_logit_noint_pro <- glm(formulas$f_logit_noint,
                              data = ey_data,
                              family = binomial(link = "probit"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_logit_noint_pro))
})
check_yhat_means(ey_space, ey_time, fit_glm_logit_noint_pro, ey_data)

### f_logit_f (logit) ----
fit_glm_logit_f_lo <- glm(formulas$f_logit_f,
                       data = ey_data,
                       family = binomial(link = "logit"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_logit_f_lo))
})
check_yhat_means(ey_space, ey_time, fit_glm_logit_f_lo, ey_data)

### f_logit_f (probit) ----
fit_glm_logit_f_pro <- glm(formulas$f_logit_f,
                       data = ey_data,
                       family = binomial(link = "probit"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_logit_f_pro))
})
check_yhat_means(ey_space, ey_time, fit_glm_logit_f_pro, ey_data)

### f_logit_noint_f (logit) ----
fit_glm_logit_noint_f_lo <- glm(formulas$f_logit_noint_f,
                             data = ey_data,
                             family = binomial(link = "logit"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_logit_noint_f_lo))
})
check_yhat_means(ey_space, ey_time, fit_glm_logit_noint_f_lo, ey_data)

### f_logit_noint_f (probit) ----
fit_glm_logit_noint_f_pro <- glm(formulas$f_logit_noint_f,
                                data = ey_data,
                                family = binomial(link = "probit"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_logit_noint_f_pro))
})
check_yhat_means(ey_space, ey_time, fit_glm_logit_noint_f_pro, ey_data)



### f_pois ----
fit_glm_pois <- glm(formulas$f_pois,
                    data = ey_data,
                    family = poisson(link = "log"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_pois))
})
check_yhat_means(ey_space, ey_time, fit_glm_pois, ey_data)

### f_pois_f ---
fit_glm_pois_f <- glm(formulas$f_pois_f,
                      data = ey_data,
                      family = poisson(link = "log"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_pois_f))
})
check_yhat_means(ey_space, ey_time, fit_glm_pois_f, ey_data)

### f_pois_off_wo (offset) ---
fit_glm_pois_off_wo_offset <- glm(formulas$f_pois_off_wo,
                               data = ey_data,
                               offset = offset,
                               family = poisson(link = "log"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_pois_off_wo_offset))
})
check_yhat_means(ey_space, ey_time, fit_glm_pois_off_wo_offset, ey_data)

### f_pois_off_wi  ---
fit_glm_pois_off_wi <- glm(formulas$f_pois_off_wi,
                        data = ey_data,
                        family = poisson(link = "log"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_pois_off_wi))
})
check_yhat_means(ey_space, ey_time, fit_glm_pois_off_wi, ey_data)

### f_qpois_off_wo ---
fit_glm_qpois_off_wo_offset <- glm(formulas$f_qpois_off_wo,
                                   data = ey_data,
                                   family = quasipoisson(link = "log"),
                                   offset = offset)
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_qpois_off_wo_offset))
})
check_yhat_means(ey_space, ey_time, fit_glm_qpois_off_wo_offset, ey_data, n_samples = 1000)


### f_qpois_off_wi ---
fit_glm_qpois_off_wi <- glm(formulas$f_qpois_off_wo,
                            data = ey_data,
                            family = quasipoisson(link = "log"))
test_that("Of class glm", {
  expect_true("glm" %in% class(fit_glm_qpois_off_wi))
})
check_yhat_means(ey_space, ey_time, fit_glm_qpois_off_wi, ey_data, n_samples = 1000)
