

nice_tc <- data.frame(date_label = c("2010", "2011", "2012"),
                      start_date = as.Date(c("2010-01-01",
                                             "2011-01-01",
                                             "2012-01-01")),
                      end_date = as.Date(c("2011-01-01",
                                           "2012-01-01",
                                           "2013-01-01")),
                      stringsAsFactors = FALSE)

numeric_tc <- nice_tc
numeric_tc[[1]] <- 2010:2012
evil_tc <- nice_tc
evil_tc[[1]] <- c(3, 2, 4)

why_tc <- nice_tc[1:2, ]
why_tc[[1]] <- c(TRUE, FALSE)

test_that("parse_force works with single logical input", {
  all_true <- rep(TRUE, nrow(nice_tc))
  all_false <- !all_true
  expect_equivalent(parse_force(TRUE, nice_tc), all_true)
  expect_equivalent(parse_force(FALSE, nice_tc), all_false)

  expect_equivalent(parse_force(TRUE, numeric_tc), all_true)
  expect_equivalent(parse_force(FALSE, numeric_tc), all_false)

  expect_equivalent(parse_force(TRUE, evil_tc), all_true)
  expect_equivalent(parse_force(FALSE, evil_tc), all_false)

  expect_error(parse_force(TRUE, why_tc), "unambiguously parse")
  expect_error(parse_force(FALSE, why_tc), "unambiguously parse")
})

test_that("parse_force works with repeated logical input", {
  f_1 <- c(T, F, T)
  expect_equivalent(parse_force(f_1, nice_tc), f_1)
  expect_equivalent(parse_force(f_1, numeric_tc), f_1)
  expect_equivalent(parse_force(f_1, evil_tc), f_1)

  expect_error(parse_force(c(T, F), why_tc), "unambiguously parse")
  expect_error(parse_force(c(F, T), why_tc), "unambiguously parse")
})

test_that("parse_force works with indexing", {
  f_1 <- c(1, 3)
  out_1 <- c(T, F, T)
  expect_equivalent(parse_force(f_1, nice_tc), out_1)
  expect_equivalent(parse_force(f_1, numeric_tc), out_1)
  expect_equivalent(parse_force(f_1, evil_tc), out_1)

  f_2 <- c(-1, -2)
  out_2 <- c(F, F, T)
  expect_equivalent(parse_force(f_2, nice_tc), out_2)
  expect_equivalent(parse_force(f_2, numeric_tc), out_2)
  expect_equivalent(parse_force(f_2, evil_tc), out_2)

  expect_error(parse_force(c(2, 3), evil_tc), "unambiguously parse")
  expect_equivalent(parse_force(c(2, 3), nice_tc), c(F, T, T))
})

test_that("parse_force works with labels", {
  expect_equivalent(parse_force(c("2010", "2012"), nice_tc), c(T, F, T))

  expect_equivalent(parse_force(c(2010, 2012), numeric_tc), c(T, F, T))

  expect_equivalent(parse_force(c(3, 4), evil_tc), c(T, F, T))
  expect_error(parse_force(c(3, 2), evil_tc),
               "unambiguously parse")

  expect_error(parse_force(c(TRUE), why_tc),
               "unambiguously parse")

  expect_error(parse_force(c("2019", "2011"), nice_tc),
               "Unable to parse")

})


