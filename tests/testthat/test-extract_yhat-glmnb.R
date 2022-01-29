test_that("glm.nb + f_qpois_off_wi", {
  if (!mass_available) skip("MASS not available")
  fit_glmnb_qpois_off_wi <- MASS::glm.nb(formulas$f_qpois_off_wi,
                                         data = ey_data)
  extract_yhat(fit_glmnb_qpois_off_wi, ey_newdata)
  test_that("Of class negbin", {
    expect_true("negbin" %in% class(fit_glmnb_qpois_off_wi))
  })
  check_yhat_means(ey_space, ey_time, fit_glmnb_qpois_off_wi, ey_data, n_samples = 5000)
  expect_true(TRUE)
})

