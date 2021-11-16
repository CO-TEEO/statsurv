### lm ----
fit_lm <- lm(formulas$f_lm,
              data = ey_data)
test_that("extract_yhat gives an error if no data", {
  expect_error(extract_yhat(ey_space, ey_time, formulas$f_lm))
})
test_that("Of class lm", {
  expect_true("lm" %in% class(fit_lm))
})
check_yhat_means(ey_space, ey_time, fit_lm, ey_data)


### lm_noint ----
fit_lm_noint <- lm(formulas$f_lm_noint,
                  data = ey_data)
test_that("Of class lm", {
  expect_true("lm" %in% class(fit_lm_noint))
})
check_yhat_means(ey_space, ey_time, fit_lm_noint, ey_data)


### lm_f ----
fit_lm_f <- lm(formulas$f_lm_f,
               data = ey_data)
test_that("Of class lm", {
  expect_true("lm" %in% class(fit_lm_noint))
})
check_yhat_means(ey_space, ey_time, fit_lm_f, ey_data)


