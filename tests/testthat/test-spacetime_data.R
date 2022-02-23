test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Test that...
# 1. we always get a valid frame back after padding
# 2. Throw an error on mal-formed inputs (missing columns, or not integer-ish)
# 3. We pad skips
# 4. We pad missing combinations
# 5. We never remove rows
check_combos <- function(spacetime_data, expected_combos) {
  combos <- spacetime_data[, c("id_space", "id_time")] %>%
    dplyr::arrange(id_space, id_time)
  expected_combos <- expected_combos[, c("id_space", "id_time")] %>%
    dplyr::arrange(id_space, id_time)

  expect_equal(combos, expected_combos)
  expect_error(validate_spacetime_data(spacetime_data), NA)
}

test_that("After padding, have all combination of inputs", {
  expected_combos <- expand.grid(id_time = 1:3,
                                 id_space = 1:3)
  ok_data <- data.frame(id_space = c(1, 3, 3),
                        id_time = c(1, 2, 3),
                        y = c("a", "b", "c"))
  check_combos(pad_spacetime_data(ok_data), expected_combos)

  ok_data <- data.frame(id_space = c(1, 3),
                        id_time = c(1, 3),
                        y = c("a", "b"))
  check_combos(pad_spacetime_data(ok_data), expected_combos)

  ok_data <- data.frame(id_space = c(1, 2, 3, 1, 2, 3),
                        id_time = c(1, 1, 1, 2, 2, 3),
                        y = runif(6))
  check_combos(pad_spacetime_data(ok_data), expected_combos)
})

test_that("We don't remove rows", {
  ok_data <- data.frame(id_space = c(1, 1, 1),
                        id_time = c(1, 2, 2),
                        y = c("a", "b", "c"))
  expect_equal(ok_data, pad_spacetime_data(ok_data))

  ok_data <- data.frame(id_space = c(1, 1, 1, 1),
                        id_time = c(1, 2, 2, 4),
                        y = c("a", "b", "c", "d"))
  expected <- data.frame(id_space = 1, id_time = c(1, 2, 2, 3, 4))
  check_combos(pad_spacetime_data(ok_data), expected)
})


test_that("Gives an error on mal-formed inputs", {
  bad_data <- data.frame(id_space = c(1, 2, 3),
                         y = c("a", "b", "c"))
  expect_error(pad_spacetime_data(bad_data), "id_time")

  bad_data <- data.frame(id_time = c(1, 2, 3),
             y = c("a", "b", "c"))
  expect_error(pad_spacetime_data(bad_data), "id_space")

  bad_data <- data.frame(x = c(1, 2, 3),
                         y = c("a", "b", "c"))
  expect_error(pad_spacetime_data(bad_data), "id_space")

  bad_data <- data.frame(id_space = c(1, 2, 3),
                         id_time = c(1.4, 2.1, 3),
                         y = c("a", "b", "c"))
  expect_error(pad_spacetime_data(bad_data), "id_time")

  bad_data <- data.frame(id_time = c(1, 2, 3),
                         id_space = c(1.4, 2.1, 3),
                         y = c("a", "b", "c"))
  expect_error(pad_spacetime_data(bad_data), "id_space")
})

test_that("Doesn't give an error on skips", {
  ok_data <- data.frame(id_time = c(1, 3, 4),
                        id_space = c(1, 2, 3),
                        y = c("a", "b", "c"))
  expect_error(pad_spacetime_data(ok_data), NA)

  ok_data <- data.frame(id_time = c(1, 3, 4),
                        id_space = c(1, 9, 3),
                        y = c("a", "b", "c"))
  expect_error(pad_spacetime_data(ok_data), NA)
})
