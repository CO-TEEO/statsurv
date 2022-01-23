set.seed(54515529)

x = rnorm(100)
spacetime_data <- data.frame(id_space = rep(1:10, each = 10),
                             id_time = rep(1:10, 10),
                             x = x,
                             y = 2.04 * x + 1.23)

test_that("When we window, filter out the data that's not complete", {
  for (ii in 1:100) {
    n <- sample(1:5, size = 1)
    mt <- sample(1:(10 - n), size = 1)

    windowed_df <- window_idtime(spacetime_data, min_train = mt, n_predict = n, model_arity = "multi")
    expect_equal(min(windowed_df$id_time), mt + n)
    # Also need to check that we have the right data
  }
})

# Need to check behavoir for model_arity = "uni"
# Need to check on some weird column types maybe? I guess it's worth check that curr_data is
# always identical to a filtered version of spacetime_data
