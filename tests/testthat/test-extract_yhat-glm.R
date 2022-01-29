glm_combos <- tibble::tribble(~f_name, ~link_name, ~offset,
                              "f_lm", "gaussian", FALSE,
                              "f_lm_noint", "gaussian", FALSE,
                              "f_lm_f", "gaussian", FALSE,
                              "f_logit", c("logit", "probit"), FALSE,
                              "f_logit_trans", c("logit", "probit"), FALSE,
                              "f_logit_noint", c("logit", "probit"), FALSE,
                              "f_logit_f", c("logit", "probit"), FALSE,
                              "f_logit_noint_f", c("logit", "probit"), FALSE,
                              "f_pois", "poisson", FALSE,
                              "f_pois_f", "poisson", FALSE,
                              "f_pois_off_wo", "poisson", TRUE,
                              "f_pois_off_wi", "poisson", FALSE,
                              "f_qpois_off_wo", "quasipoisson", TRUE,
                              "f_qpois_off_wi", "quasipoisson", FALSE) %>%
  tidyr::unnest_longer("link_name")

link_lookup <- list(gaussian = gaussian(link = "identity"),
                    logit = binomial(link = "logit"),
                    probit = binomial(link = "probit"),
                    poisson = poisson(link = "log"),
                    quasipoisson = quasipoisson(link = "log"))

for (ind in seq_len(nrow(glm_combos))) {
  curr_row <- glm_combos[ind, , drop = FALSE]
  if (curr_row$offset) {
    fit <- glm(formulas[[curr_row$f_name]],
               data = ey_data,
               offset = offset, #defined in setup-extract_yhat.R
               family = link_lookup[[curr_row$link_name]])
  } else {
  fit <- glm(formulas[[curr_row$f_name]],
             data = ey_data,
             family = link_lookup[[curr_row$link_name]])
  }
  test_that("extract_yhat matches augment for glm models", {
    compare_extract_aug(fit, ey_newdata)
  })
}

test_that("extract_yhat matches augment for glm.nb models", {
  if (!mass_available) skip("MASS not available")

  fit <- MASS::glm.nb(formulas$f_qpois_off_wi,
                      data = ey_data)
  suppressWarnings(compare_extract_aug(fit, ey_newdata))
})


