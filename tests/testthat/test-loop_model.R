set.seed(65616181)

# We're going to set up a very basic model and then try to break it
space_coord <- data.frame(space_label = c("space1", "space2", "space3"),
                          stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()



start_times <- 1:12
fin_times <- start_times + 1L
labels <- paste0("X", start_times) #sapply(start_times + 64, intToUtf8)
time_coord <- data.frame(time_labels = labels,
                         time = start_times,
                         fin_time = fin_times,
                         stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()
data_for_model <- gridcoord::gc_expand(time_coord, space_coord)
data_for_model$y <- data_for_model$time * 1.05 + rnorm(n = nrow(data_for_model))


spacetime_data <- data_for_model %>%
  dplyr::mutate(id_space = as.numeric(factor(space_label)),
         id_time = time) %>%
  tibble::as_tibble() %>%
  dplyr::select(id_space, id_time, time, fin_time, y)

simple_lm_func <- function(data_for_model) {
  fit <- lm(y ~ time,
            data = data_for_model)
  return(fit)
}

medium_lm_func <- function(data_for_model) {
  fit <- lm(y ~ time + factor(id_space),
            data = data_for_model)
  return(fit)
}

get_unique_tc <- function(spacetime_data) {
  res <- lapply(spacetime_data$curr_data,
         function(x) {
           if (is.null(x)) {
             return(NULL)
           } else {
             return(unique(x$id_time))
           }
         })
  res[!vapply(res, is.null, logical(1))]
}


test_that("loop_model behaves consistently", {
  fits_and_data <- loop_model(spacetime_data, "y", simple_lm_func,
                              model_arity = "multi",
                              min_train = 7)
  expect_known_value(fits_and_data, file = "simple_lm_output.RDS")
})

test_that("loop_model behaves consistently if exploded", {
  fits_and_data <- loop_model(spacetime_data, "y", simple_lm_func,
                              model_arity = "uni",
                              min_train = 7)
  expect_known_value(fits_and_data, file = "simple_lm_output_exploded.RDS")

})



test_that("data windowing behaves corrects", {
  # Our window grows as we go forward in time
  fits_and_data <- loop_model(spacetime_data, "y", simple_lm_func,
                              model_arity = "multi",
                              min_train = 7)
  all_uniq_tc <- get_unique_tc(fits_and_data)
  # good_ones <- all_uniq_t/c[!vapply(all_uniq_tc, is.null, logical(1))]

  expect_equal(all_uniq_tc[[1]], 1:8)
  expect_equal(all_uniq_tc[[2]], 1:9)
  expect_equal(all_uniq_tc[[5]], 1:12)

  # Out window stops growing when it hits max_train
  all_uniq_tc <- loop_model(spacetime_data, "y", simple_lm_func,
                            model_arity = "multi",
                            min_train = 7, max_train = 10) %>%
    get_unique_tc()
  expect_equal(all_uniq_tc[[1]], 1:8)
  expect_equal(all_uniq_tc[[2]], 1:9)
  expect_equal(all_uniq_tc[[5]], 2:12)

  # We can step forward in chunks, if it's not an equal-subdivision of time_coord
  # I haven't implemented this
  all_uniq_tc <- loop_model(spacetime_data, "y", simple_lm_func,
                            model_arity = "multi",
                            min_train = 7, max_train = 7, n_predict = 2, step = 2) %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.null(model_fit)) %>%
    dplyr::ungroup() %>%
    get_unique_tc()
  expect_equal(length(all_uniq_tc), 2)
  expect_equal(all_uniq_tc[[1]], 1:9)
  expect_equal(all_uniq_tc[[2]], 3:11)

  # We can step forward in chunks, if it is an equal-subdivision of time_coord
  all_uniq_tc <- loop_model(spacetime_data, "y", simple_lm_func,
                            model_arity = "multi",
                            min_train = 8, max_train = 8, n_predict = 2, step = 2) %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.null(model_fit)) %>%
    dplyr::ungroup() %>%
    get_unique_tc()
  expect_equal(length(all_uniq_tc), 2)
  expect_equal(all_uniq_tc[[1]], 1:10)
  expect_equal(all_uniq_tc[[2]], 3:12)

  # We can step forward in just one chunk
  all_uniq_tc <- loop_model(spacetime_data, "y", simple_lm_func,
                            model_arity = "multi",
                            min_train = 10, n_predict = 2, step = 2) %>%
    get_unique_tc()
  expect_equal(length(all_uniq_tc), 1)
  expect_equal(all_uniq_tc[[1]], 1:12)
})





test_that("Passing list-arguments into our model works correctly", {
  weird_fit_fun <- function(data_for_model, list_arg, other_arg) {
    force(list_arg)
    force(other_arg)
    list_arg[[1]] <- list_arg[[1]] + other_arg
    fit <- lm(y ~ time,
              data = data_for_model)
    # test comment
    return(fit)
  }
  arg1 <- list(a = 14, b = 22)
  arg2 <- 104
  all_results <- loop_model(spacetime_data, "y", weird_fit_fun,
                            list_arg = arg1, other_arg = arg2)

})

test_that("We can use the data-prep function", {
  data_prep_fun <- function(curr_data) {
    curr_data <- curr_data %>%
      dplyr::mutate(mean_y = mean(y))
    # over_7_flag <- mean(curr_data$y) > 7
    return(list(curr_data))
  }

  fit_fun <- function(data_for_model, use_mean = FALSE) {
    if (use_mean) {
      fit <- lm(y ~ time + mean_y,
                data = data_for_model)
    } else {
      fit <- lm(y ~ time,
                data = data_for_model)
    }
    return(fit)
  }
  loop_model(spacetime_data, "y", fit_fun, data_prep_fun)

})


test_that("We can modify data inside the model function and use it later", {
  data_prep_fun <- function(data_for_model) {
    data_for_model$t_4 <- pmax(data_for_model$time - 4, 0)
    data_for_model
    }

  mod_lm_func <- function(data_for_model) {
    fit <- lm(y ~ time + t_4,
              data = data_for_model)
    return(fit)
  }
  fits_and_data <- loop_model(spacetime_data, "y", mod_lm_func, data_prep_fun,
                              model_arity = "multi",
                              min_train = 7)
  # all_data <- fits_and_data[[2]]
  # curr_data <- all_data[["X9"]][all_data[["X9"]]$space_label == "space1", ]
  # t_4 <- c(0, 0, 0, 0, 1, 2, 3, 4, 5)
  # expect_equal(curr_data$t_4, t_4)

  fits_and_data <- loop_model(spacetime_data, "y", mod_lm_func, data_prep_fun,
                             model_arity = "uni",
                              min_train = 7)
  # curr_data <- fits_and_data[[2]][["space2", "X9"]]
  # expect_equal(curr_data$t_4, t_4)
})


#I'd like to
#   Throw an error for unnamed extra arguments? Maybe that's too strict. What if I want to be able to put in ... into it?
#   Throw an error on name mis-match - that's something we should be able to do.
#
# Ok, what are the parts of this that I want to test?
# -1. The windowing functions - weird edge cases of min/max
# -2. Mis-formed space and time coordinates --- I don't think I need to check all the edge cases here, just
# that it throws an error if it's not a valid gc, and even if it is a valid gc but it doesn't line up
# with the data you put in (think about if that's the behavoir we even want)
# 3. Seperately, I need to test our spatial dispatch.
# -4. Getting extra arguments into and out of our model - including if we have some weird ones, like a
#   model that takes a list of things as an argument, or a model that returns (fit, data, list_of_stuff)
# -5. What happens if data_for_model isn't a complete grid, and we have missing rows? Should I add a gridcoord::gc_pad feature?
#   I'm not sure how that would work, but it might be convenient. Or maybe what I want is to filter it down a couple steps.
#   Hrm. I think most models are able to deal with it. I guess I might run into issues
# -6. Modified data_for_model inside the function - that works correctly, whatever that means.
