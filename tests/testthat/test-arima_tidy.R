
compare_arimas <- function(old, new) {
  expect_equal(unname(old$coef), unname(new$coef))
  expect_equal(unname(old$var.coef), unname(new$var.coef))
  expect_equal(old$model, new$model)
}

compare_preds <- function(old, new) {
  expect_equal(as.numeric(old$pred), as.numeric(new$pred))
}

# Taking some examples from the arima documentation to see if we match
test_that("Lake Huron comparison works", {
  LakeHuron_df <- data.frame(lake_level = as.numeric(LakeHuron),
                             year = seq(start(LakeHuron)[[1]],
                                        end(LakeHuron)[[1]],
                                        by = 1/frequency(LakeHuron))) %>%
    dplyr::mutate(year_c = year - 1920)

  old <- arima(LakeHuron, order = c(2,0,0), xreg = time(LakeHuron) - 1920)
  new <- arima_tidy(lake_level ~ I(year - 1920), data = LakeHuron_df, order = c(2, 0, 0))
  compare_arimas(old, new)
})

make_df <- function(ts) {
  f <- frequency(ts)
  s <- start(ts)
  e <- end(ts)
  if (f == 1) {
    data.frame(y = as.numeric(ts),
               date = seq(s[[1]],
                          e[[1]],
                          by = 1))
  } else
    data.frame(y = as.numeric(ts),
             date = seq(s[[1]] + s[[2]]/f,
                        e[[1]] + e[[2]]/f,
                        by = 1/f))
}
test_that("presidents comparison works (has NA's)", {
  presidents_df <- make_df(presidents)
  old <- arima(presidents, c(1, 0, 0))
  new <- arima_tidy(y ~ 1, data = presidents_df, order = c(1, 0, 0))
  compare_arimas(old, new)

  old <- arima(presidents, c(3, 0, 0))
  new <- arima_tidy(y ~ 1, data = presidents_df, order = c(3, 0, 0))
  compare_arimas(old, new)

  old <- arima(presidents, order=c(2,0,1), seasonal=c(1,0,0),
                   fixed=c(NA, NA, 0.5, -0.1, 50), transform.pars=FALSE)

  new <- arima_tidy(y ~ 1, data = presidents_df, order=c(2,0,1),
                    seasonal= list(order = c(1,0,0), period = 4), #have to manually give the period
               fixed=c(NA, NA, 0.5, -0.1, 50), transform.pars=FALSE)
  compare_arimas(old, new)
})


LakeHuron_df <- make_df(LakeHuron) %>%
  dplyr::mutate(is_election_year = date %% 4 == 0,
                is_mod5 = date %% 5 == 0)

test_that("Weird formulas work", {
  expect_error(arima_tidy(y ~ date + is_election_year, data = LakeHuron_df, order = c(2,0,0)),
               NA)
  new <- arima_tidy(sqrt(y) ~ I(date - 1920), data = LakeHuron_df, order = c(2, 0, 0))
  sqrt_ts <- ts(data = sqrt(LakeHuron_df$y), start = 1876)
  old <- arima(sqrt_ts, order = c(2, 0, 0), xreg = time(LakeHuron) - 1920)
  compare_arimas(old, new)

  expect_error(
    arima_tidy(sqrt(y) ~ date + is_election_year + is_mod5,
               data = LakeHuron_df,
               order = c(2, 0, 0)),
    NA)

  expect_error(
    arima_tidy(sqrt(y) ~ date + is_election_year*is_mod5,
               data = LakeHuron_df,
               order = c(2, 0, 0)),
    NA)

  expect_error(
    arima_tidy(sqrt(y) ~ date + is_election_year + is_election_year:is_mod5,
               data = LakeHuron_df,
               order = c(2, 0, 0)),
    NA)


  expect_error(
    arima_tidy(sqrt(y) ~ log(date),
               data = LakeHuron_df,
               order = c(2, 0, 0)),
    NA)

})

test_that("We can reference variables outside of data", {
  shift <- 2000
  expect_error(
    arima_tidy(sqrt(y) ~ log(date + shift),
             data = LakeHuron_df,
             order = c(2, 0, 0)),
    NA)
})

test_that("Predict matches", {
  presidents_df <- make_df(presidents)
  old <- arima(presidents, c(1, 0, 0))
  new <- arima_tidy(y ~ 1, data = presidents_df, order = c(1, 0, 0))
  compare_preds(predict(old, n.ahead = 2), predict(new, n.ahead = 2))
})

test_that("Predict with xreg matches", {
  skip_if_not_installed("scanstatistics")

  data(NM_popcas)
  santa_fe <- NM_popcas %>%
    dplyr::filter(county == "santafe")
  rows <- seq(1, nrow(santa_fe) - 2)
  pred_rows <- seq(nrow(santa_fe) - 1, nrow(santa_fe))
  xreg = as.matrix(santa_fe[, "population", drop = FALSE])
  fit_stats_arima <- stats::arima(santa_fe$count[rows],
                                  order = c(2, 1, 0),
                                  xreg = xreg[-pred_rows, , drop = FALSE])

  op <- predict(fit_stats_arima, newxreg = new_xreg[pred_rows, ])

  fit_arima_tidy <- arima_tidy(count ~ population, data = santa_fe[-pred_rows, ], order = c(2, 1, 0))
  np <- predict(fit_arima_tidy, only_newdata = santa_fe[pred_rows, ])
  compare_preds(op, np)

  np2 <- predict(fit_arima_tidy, newxreg = xreg[pred_rows, ])
  compare_preds(op, np2)
})
