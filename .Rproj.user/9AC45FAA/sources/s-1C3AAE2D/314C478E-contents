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

simple_lm_func <- function(space_coord, time_coord, data_for_model) {
  fit <- lm(y ~ time,
            data = data_for_model)
  return(list(fit = fit,
              data = data_for_model))
}

medium_lm_func <- function(space_coord, time_coord, data_for_model) {
  fit <- lm(y ~ time + factor(space_label),
            data = data_for_model)
  return(list(fit = fit,
              data = data_for_model))
}

get_unique_tc <- function(all_fits_and_data) {
  res <- lapply(all_fits_and_data[[2]],
         function(x) {
           if (length(x) == 1 && is.na(x)) {
             return(NULL)
           } else {
             return(unique(x$time))
           }
         })
  res[!vapply(res, is.null, logical(1))]
}

just_windows <- function(min_train, max_train = Inf, n_predict = 1) {
  loop_model(space_coord, time_coord, data_for_model,
              "y", simple_lm_func, model_arity = "multi",
              use_cache = FALSE, verbose = FALSE,
              min_train = min_train, max_train = max_train, n_predict = 1)
}

just_coord_data <- function(space_coord, time_coord, data_for_model) {
  suppressWarnings(loop_model(space_coord, time_coord, data_for_model,
             "y", simple_lm_func, model_arity = "multi", verbose = FALSE,
             use_cache = FALSE, min_train = 7, n_predict = 2))
}

check_exlist <- function(gridlist, inner_value) {
  expect_true(is_type(gridlist, "gridlist"))
  for (time_ex in gridlist) {
    for (ex in time_ex) {
      if (length(ex) == 1 && is.na(ex)) {
        next
      }
      expect_equal(ex, inner_value)
    }
  }
}

test_that("loop_model behaves consistently", {
  fits_and_data <- loop_model(space_coord, time_coord, data_for_model,
                                 "y", simple_lm_func,  model_arity = "multi", use_cache =  FALSE,
                              min_train = 7, verbose = FALSE)
  expect_known_value(fits_and_data, file = "simple_lm_output.RDS")

  d1 <- fits_and_data[[2]][["X8"]]
  d2 <- data_for_model[data_for_model$time <= 8, ]
  rownames(d1) <- NULL
  rownames(d2) <- NULL
  expect_equal(d1, d2)
})

test_that("loop_model behaves consistently if exploded", {
  fits_and_data <- loop_model(space_coord, time_coord, data_for_model,
                              "y", simple_lm_func,  model_arity = "uni", use_cache =  FALSE,
                              min_train = 7, verbose = FALSE)
  expect_known_value(fits_and_data, file = "simple_lm_output_exploded.RDS")

})

test_that("We run a model if a space coord is a df, a spdf, or an sf", {
  # And all of them should really give the same result
  space_coord_spdf <- rgdal::readOGR("three_zips/three_zips.shp",
                                     verbose = FALSE,
                                     stringsAsFactors = FALSE)
  space_coord_df <- space_coord_spdf@data
  space_coord_sf <- sf::st_read("three_zips/three_zips.shp",
                                quiet = TRUE,
                                stringsAsFactors = FALSE)
  all_res <- list()
  ind <- 0

  curr_data <- gridcoord::gc_expand(time_coord, space_coord_df)
  y <- curr_data$time * 1.05 + rnorm(n = nrow(curr_data))
  for (sc in list(space_coord_spdf, space_coord_df, space_coord_sf)) {
    curr_data <- gridcoord::gc_expand(time_coord, sc)
    curr_data$y <- y
    curr_res <- loop_model(sc, time_coord, curr_data,
                           "y", simple_lm_func,  model_arity = "multi", use_cache =  FALSE,
                           min_train = 7, verbose = FALSE)
    ind <- ind + 1
    all_res[[ind]] <- curr_res
  }
  expect_equal(all_res[[2]], all_res[[1]])
  expect_equal(all_res[[3]], all_res[[2]])

  all_res <- list()
  ind <- 0
  for (sc in list(space_coord_spdf, space_coord_df, space_coord_sf)) {
    curr_data <- gridcoord::gc_expand(time_coord, sc)
    curr_data$y <- y
    curr_res <- loop_model(sc, time_coord, curr_data,
                           "y", simple_lm_func,  model_arity = "uni", use_cache =  FALSE,
                           min_train = 7, verbose = FALSE)
    ind <- ind + 1
    all_res[[ind]] <- curr_res
  }
  expect_equivalent(all_res[[2]], all_res[[1]]) # The space coordinates are different, so compare
  # with expect_equivalent
  expect_equivalent(all_res[[3]], all_res[[2]])

})
test_that("data windowing behaves corrects", {
  # Our window grows as we go forward in time
  fits_and_data <- loop_model(space_coord, time_coord, data_for_model,
                              "y", simple_lm_func,  model_arity = "multi", use_cache =  FALSE,
                              min_train = 7, verbose = FALSE)
  all_uniq_tc <- get_unique_tc(fits_and_data)
  # good_ones <- all_uniq_t/c[!vapply(all_uniq_tc, is.null, logical(1))]
  expect_equal(names(all_uniq_tc), paste0("X", 8:12))
  expect_equal(all_uniq_tc[["X8"]], 1:8)
  expect_equal(all_uniq_tc[["X9"]], 1:9)
  expect_equal(all_uniq_tc[["X12"]], 1:12)

  # Out window stops growing when it hits max_train
  all_uniq_tc <- loop_model(space_coord, time_coord, data_for_model,
                            "y", simple_lm_func,  model_arity = "multi", use_cache =  FALSE,
                            min_train = 7, max_train = 10, verbose = FALSE) %>%
    get_unique_tc()
  expect_equal(names(all_uniq_tc), paste0("X", 8:12))
  expect_equal(all_uniq_tc[["X8"]], 1:8)
  expect_equal(all_uniq_tc[["X9"]], 1:9)
  expect_equal(all_uniq_tc[["X12"]], 2:12)

  # We can step forward in chunks, if it's not an equal-subdivision of time_coord
  all_uniq_tc <- loop_model(space_coord, time_coord, data_for_model,
                            "y", simple_lm_func,  model_arity = "multi", use_cache =  FALSE,
                            min_train = 7, max_train = 8, n_predict = 3, verbose = FALSE) %>%
    get_unique_tc()
  expect_equal(names(all_uniq_tc), c("X10", "X12"))
  expect_equal(all_uniq_tc[["X10"]], 1:10)
  expect_equal(all_uniq_tc[["X12"]], 3:12)

  # We can step forward in chunks, if it is an equal-subdivision of time_coord
  all_uniq_tc <- loop_model(space_coord, time_coord, data_for_model,
                            "y", simple_lm_func,  model_arity = "multi", use_cache =  FALSE,
                            min_train = 8, max_train = 8, n_predict = 2, verbose = FALSE) %>%
    get_unique_tc()
  expect_equal(names(all_uniq_tc), c("X10", "X12"))
  expect_equal(all_uniq_tc[["X10"]], 1:10)
  expect_equal(all_uniq_tc[["X12"]], 3:12)

  # We can step forward in just one chunk
  all_uniq_tc <- loop_model(space_coord, time_coord, data_for_model,
                            "y", simple_lm_func,  model_arity = "multi", use_cache =  FALSE,
                            min_train = 10, n_predict = 3, verbose = FALSE) %>%
    get_unique_tc()
  expect_equal(names(all_uniq_tc), c("X12"))
  expect_equal(all_uniq_tc[["X12"]], 1:12)
})

test_that("data windowing throws an error if we call it with non-sensical arguments", {
  expect_error(just_windows(min_train = "A"), class = "error_type")
  expect_error(just_windows(min_train = 0), class = "error_bad_arg_value")
  expect_error(just_windows(min_train = nrow(time_coord)), class = "error_bad_arg_value")
  expect_error(just_windows(min_train = 7, max_train = 4), class = "error_bad_arg_value")
  expect_error(just_windows(min_train = -1, max_train = 0), class = "error_bad_arg_value")
  expect_error(just_windows(min_train = 8, max_train  = "A"), class = "error_type")
  # I don't think n_predict should affect any of these results - we figure out how many things to
  # train on, and then predict on the remainds
})

test_that("Throws error if colnames or labels don't match between coordinates and data_for_model", {
  # Ok, we do have to think about this
  # We have the problem that we have no way change the time_coord and space_coord in the
  # top-level script. So we should be very careful with any modifications we have to make in the
  # coordinate.
  # But, ok, the basic thing
  space_coord_badcolname <- data.frame("bad_space_title" = c("space1", "space2", "space3"),
                                       stringsAsFactors = FALSE) %>%
    gridcoord::gc_gridcoord()
  space_coord_badlabels <- data.frame("space_label" = c("space10", "space20", "space30"),
                                stringsAsFactors = FALSE) %>%
    gridcoord::gc_gridcoord()

  time_coord_badcolname <- cbind(data.frame("bad_time_title" = time_coord$time_labels,
                                     stringsAsFactors = FALSE),
                          time_coord[, 2:3])

  time_coord_badlabels <- time_coord
  time_coord_badlabels$time_labels <- paste0("X", time_coord$time_labels)

  expect_error(just_coord_data(space_coord_badcolname, time_coord, data_for_model),
               class = "error_bad_arg_value")
  expect_error(just_coord_data(time_coord_badcolname, time_coord, data_for_model),
               class = "error_bad_arg_value")
  expect_error(just_coord_data(space_coord_badlabels, time_coord, data_for_model),
               class = "error_bad_arg_value")
  expect_error(just_coord_data(space_coord, time_coord_badlabels, data_for_model),
               class = "error_bad_arg_value")

  data_for_model_unequal <- data_for_model[-(33:36), ]
  expect_error(just_coord_data(space_coord, time_coord, data_for_model_unequal),
               class = "error_bad_arg_value")
})

test_that("Passing list-arguments into and out of our model works correctly", {
  weird_fit_fun <- function(space_coord, time_coord, data_for_model, list_arg, other_arg) {
    force(list_arg)
    force(other_arg)
    list_arg[[1]] <- list_arg[[1]] + other_arg
    fit <- lm(y ~ time,
              data = data_for_model)
    # test comment
    return(list(fit = fit,
                data = data_for_model,
                extra = list_arg,
                out4 = 15))

  }
  arg1 <- list(a = 14, b = 22)
  arg2 <- 104
  all_results <- loop_model(space_coord, time_coord, data_for_model, "y", use_cache = FALSE,
                            weird_fit_fun, verbose = FALSE,
                            extra_model_args = list(list_arg = arg1, other_arg = arg2))
  all_extra <- all_results[[3]]
  all_good_extra <- all_extra[!is_singular_na(all_extra)]
  for (ex in all_good_extra) {
    expect_equal(ex, list(a = 118, b = 22))
  }


  model_name <- transfer_to_file(weird_fit_fun)
  all_results <- loop_model(space_coord, time_coord, data_for_model, "y",
                            model_arity = "uni", use_cache = FALSE,
                            weird_fit_fun, verbose = TRUE,
                            extra_model_args = list(list_arg = arg1,
                                                    other_arg = arg2))
  all_extra <- all_results[[3]]
  expect_true(is_type(all_extra, "gridlist"))
  for (space_ex in all_extra) {
    for (ex in space_ex) {
      if (length(ex) == 1 && is.na(ex)) {
        next
      }
      expect_equal(ex, list(a = 118, b = 22))
    }
  }
})

test_that("We can pass 4+ arguments in and out of our model function", {
  long_fit_fun <- function(space_coord, time_coord, data_for_model, a, b, c2, d, e, f) {
    # browser()
    data_for_model$sum <- mean(a + b + c2 + d + e + f + 1)
    fit <- lm(y ~ time,
              data = data_for_model)
    out1 <- "A"; out2 <- "B"; out3 <- "C"; out4 <- "D"
    return(list(fit = fit,
                data = data_for_model,
                out1 = out1,
                out2 = out2,
                out3 = out3,
                out4 = out4))
  }
  all_results <- loop_model(space_coord, time_coord, data_for_model, "y",
                            model_arity = "uni", use_cache = FALSE,
                            long_fit_fun, verbose = FALSE,
                            extra_model_args = list(a = 10, b = 11, c2 = c(1, 3, 4),
                            d = 18, e = 103, f = -104))


  check_exlist(all_results[[3]], "A")
  check_exlist(all_results[[4]], "B")
  check_exlist(all_results[[5]], "C")
  check_exlist(all_results[[6]], "D")
})

test_that("We can modify data inside the model function and use it later", {
  mod_lm_func <- function(space_coord, time_coord, data_for_model) {
    data_for_model$t_4 <- pmax(data_for_model$time - 4, 0)
    fit <- lm(y ~ time + t_4,
              data = data_for_model)
    return(list(fit = fit,
                data = data_for_model))
  }
  fits_and_data <- loop_model(space_coord, time_coord, data_for_model,
                              "y", mod_lm_func,  model_arity = "multi", use_cache =  FALSE,
                              min_train = 7, verbose = FALSE)
  all_data <- fits_and_data[[2]]
  curr_data <- all_data[["X9"]][all_data[["X9"]]$space_label == "space1", ]
  t_4 <- c(0, 0, 0, 0, 1, 2, 3, 4, 5)
  expect_equal(curr_data$t_4, t_4)

  fits_and_data <- loop_model(space_coord, time_coord, data_for_model,
                              "y", mod_lm_func,  model_arity = "uni", use_cache =  FALSE,
                              min_train = 7, verbose = FALSE)
  curr_data <- fits_and_data[[2]][["space2", "X9"]]
  expect_equal(curr_data$t_4, t_4)
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
