library(broom)
library(broom.mixed)

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
  skip_if_not_installed("MASS")

  # Can't use offset in glm.nb models, so only have the one to check
  fit <- MASS::glm.nb(formulas$f_qpois_off_wi,
                      data = ey_data)
  suppressWarnings(compare_extract_aug(fit, ey_newdata, formulas$f_qpois_off_wi))
})

### lmer ----
lmer_formulas <- formulas_mermod[startsWith(names(formulas_mermod), "f_lm")]

for (frmla in lmer_formulas) {
  test_that("extract_yhat matches augment for lmer models", {
    skip_if_not_installed("lme4")
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
    skip_if_not_installed("lme4")
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

### INLA ----
# This ones extra-long
inla_lookup <- tibble::tribble(~link_name, ~family, ~link,
                               "gaussian", "gaussian", "identity",
                               "logit", "binomial", "logit",
                               "probit", "binomial", "probit",
                               "poisson", "poisson", "log",
                               "quasipoisson", "nbinomial", "log")


inla_combos <- dplyr::left_join(glm_combos, inla_lookup, by = "link_name")
inla_combos <- dplyr::bind_rows(dplyr::mutate(inla_combos, use_mer = TRUE),
                                dplyr::mutate(inla_combos, use_mer = FALSE))
for (ind in seq_len(nrow(inla_combos))) {
  curr_row <- inla_combos[ind, , drop = FALSE]
  if (curr_row$use_mer) {
    curr_f <- formulas_inla[[curr_row$f_name]]
    curr_data <- ey_data_mermod
    curr_newdata <- ey_newdata_mermod
  } else {
    curr_f <- formulas[[curr_row$f_name]]
    curr_data <- ey_data
    curr_newdata <- ey_newdata
  }
  if (curr_row$offset) {
    fit <- INLA::inla(curr_f,
                      data = curr_data,
                      family = curr_row$family,
                      control.family = list(link = curr_row$link),
                      control.compute = list(config = TRUE),
                      control.predictor = list(compute=TRUE),
                      offset = offset)
    yhat <- extract_yhat(fit, curr_newdata)

    fit2 <- INLA::inla(curr_f,
                       data = curr_data,
                       family = curr_row$family,
                       control.family = list(link = curr_row$link),
                       control.compute = list(config = TRUE),
                       control.predictor = list(compute=TRUE),
                       E = exposure)
    yhat2 <- extract_yhat(fit2, curr_newdata)
    expect_equal(yhat, yhat2, tolerance = 0.01)
  } else {
    fit <- INLA::inla(curr_f,
                      data = curr_data,
                      family = curr_row$family,
                      control.family = list(link = curr_row$link),
                      control.compute = list(config = TRUE),
                      control.predictor = list(compute=TRUE), verbose = FALSE)
    yhat <- extract_yhat(fit, curr_newdata)
  }
  test_that("extract_yhat.INLA gives reasonable numbers", {
    check_extract(fit, yhat, curr_f)
  })
}

test_that("extract_yhat.INLA gives a useful error if control.predictor$compute = FALSE", {
  skip_on_cran()
  skip_if_not_installed("INLA")


  bad_fit_inla <- INLA::inla(formulas$f_lm,
                             data = ey_data,
                             family = "gaussian",
                             control.compute = list(config = TRUE),
                             control.predictor = list(compute = FALSE))
  # Currently gives an error, but not a useful one.
  expect_error(extract_yhat(bad_fit_inla, ey_newdatadata),
               "control.predictor")
})
