### lm ----
lm_formulas <- formulas[startsWith(names(formulas), "f_lm")]

for (frmla in lm_formulas) {
  fit <- lm(frmla,
            data = ey_data)
  compare_extract_aug(fit, ey_newdata)
}
