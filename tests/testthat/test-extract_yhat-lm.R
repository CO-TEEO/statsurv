### lm ----
lm_formulas <- formulas[startsWith(names(formulas), "f_lm")]

for (frmla in lm_formulas) {
  fit <- lm(frmla,
            data = ey_data)
  test_that("extract_yhat matches augment for lm models", {
    compare_extract_aug(fit, ey_newdata, frmla)
  })
}

### glm ----
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
    compare_extract_aug(fit, ey_newdata, formulas[[curr_row$f_name]])
  })
}

### glm.nb ----
test_that("extract_yhat matches augment for glm.nb models", {
  if (!mass_available) skip("MASS not available")

  # Can't use offset in glm.nb models, so only have the one to check
  fit <- MASS::glm.nb(formulas$f_qpois_off_wi,
                      data = ey_data)
  suppressWarnings(compare_extract_aug(fit, ey_newdata, formulas$f_qpois_off_wi))
})

### lmer ----
lmer_formulas <- formulas_mermod[startsWith(names(formulas_mermod), "f_lm")]

for (frmla in lmer_formulas) {
  test_that("extract_yhat matches augment for lmer models", {
    if (!lme4_available) skip("lme4 not available")
    fit <- lme4::lmer(frmla,
                      data = ey_data_mermod)

    compare_extract_aug(fit, ey_newdata_mermod, frmla)
  })
}

### glmer ----
glmer_combos <- glm_combos %>%
  dplyr::filter(link_name != "gaussian",
         !startsWith(link_name, "quasi"))
for (ind in seq_len(nrow(glmer_combos))) {
  test_that("extract_yhat matches augment for glmer models", {
    if (!lme4_available) skip("lme4 not available")
    curr_row <- glmer_combos[ind, , drop = FALSE]

    if (curr_row$offset) {
      # glmer models can't handle offsets in the function call, but they don't tell you that
      # So there should be an error when we try to run extract_yhat
      fit <- lme4::glmer(formulas_mermod[[curr_row$f_name]],
                         data = ey_data_mermod,
                         offset = offset, #defined in setup-extract_yhat.R
                         family = link_lookup[[curr_row$link_name]])
      expect_error(extract_yhat(fit, ey_newdata_mermod))
    } else {
      fit <- lme4::glmer(formulas_mermod[[curr_row$f_name]],
                         data = ey_data_mermod,
                         family = link_lookup[[curr_row$link_name]])
      compare_extract_aug(fit, ey_newdata_mermod, formulas_mermod[[curr_row$f_name]])
    }

  })
}
