# What does pivot_for_scan do?
# How does it fail if we give it weird inputs?

df <- data.frame(vals = c(1, 2, 3, 4, 5, 6),
                 space = c("s1", "s2", "s3", "s1", "s2", "s3"),
                 time = c("t1", "t1", "t1", "t2", "t2", "t2"),
                 stringsAsFactors = FALSE)

expected <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = 2)
rownames(expected) <- c("t1", "t2")
colnames(expected) <- c("s1", "s2", "s3")

test_that("pivot_for_scan works", {
  # Gives a matrix, rows are time, columns are space
  res <- pivot_for_scan(df, "vals", "space", "time")
  expect_equal(res, expected)
})

test_that("Warning if non-unique", {
  non_unique_df <- df
  non_unique_df[2, "space"] <- "s1"
  expect_warning(pivot_for_scan(non_unique_df, "vals", "space", "time"),
                 "unique")
})


test_that("Pads with NAs if incomplete", {
  incomplete_df <- df[1:5, ]
  res <- pivot_for_scan(incomplete_df, "vals", "space", "time")
  expected2 <- expected
  expected2[2, 3] <- NA
  expect_equal(res, expected2)
})

test_that("Passing a gridcoord rearranges the columns", {
  space_coord <- data.frame(space = c("s2", "s3", "s1"),
                            stringsAsFactors = FALSE)
  res <- pivot_for_scan(df, "vals", space_coord, "time")
  expect_equal(res, expected[, c(2, 3, 1)])

  time_coord <- data.frame(time = c("t2", "t1"),
                           stringsAsFactors = FALSE)
  res <- pivot_for_scan(df, "vals", "space", time_coord)
  expect_equal(res, expected[c(2, 1), ])

  res <- pivot_for_scan(df, "vals", space_coord, time_coord)
  expect_equal(res, expected[c(2, 1), c(2, 3, 1)])
})

test_that("Extra rows are silently removed", {
  time_coord <- data.frame(time = c("t1", "t2", "t3"),
                           stringsAsFactors = FALSE)
  expect_equal(pivot_for_scan(df, "vals", "space", time_coord),
               expected)

  time_coord <- data.frame(time = c("t2", "t1", "t3"),
                           stringsAsFactors = FALSE)
  expect_equal(pivot_for_scan(df, "vals", "space", time_coord),
               expected[c(2, 1), ])

})

test_that("Extra columns are silently removed", {
  space_coord <- data.frame(space = c("s1", "s2", "s3", "s4"),
                            stringsAsFactors = FALSE)
  expect_equal(pivot_for_scan(df, "vals", space_coord, "time"),
               expected)

  space_coord <- data.frame(space = c("s4", "s2", "s3", "s1"),
                            stringsAsFactors = FALSE)
  expect_equal(pivot_for_scan(df, "vals", space_coord, "time"),
               expected[, c(2, 3, 1)])

})

