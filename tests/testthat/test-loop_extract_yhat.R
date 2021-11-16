set.seed(210321060)
op <- options(simplecache.debug = TRUE)

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

# Ok, the easy way to get the info we want would be to call loop_model.
# But that makes it harder to de-bug, b/c then we don't know if we made a change, is it coming from loop_extract_yhat or loop_model?
# BUt I think that's why we have tests for both.
simple_lm_output <- loop_model(space_coord, time_coord, data_for_model, "y",
                               path_to_model = simple_lm_func,
                               min_train = 7,
                               use_cache = FALSE,
                               verbose = FALSE)
simple_lm_output_exploded <- loop_model(space_coord, time_coord, data_for_model, "y",
                                        path_to_model = simple_lm_func,
                                        model_arity = "uni",
                                        min_train = 7,
                                        use_cache = FALSE,
                                        verbose = FALSE)

just_coord_data <- function(space_coord, time_coord, all_fits, all_data) {
  loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                    path_to_model = simple_lm_func,
                    use_cache = FALSE,
                    verbose = FALSE)
}

test_that("loop_extract_yhat behaves consistently", {
  all_yhat <- loop_extract_yhat(space_coord, time_coord,
                                simple_lm_output[[1]], simple_lm_output[[2]],
                                path_to_model = simple_lm_func,
                                use_cache = FALSE,
                                verbose = FALSE)
  expect_known_value(all_yhat, file = "simple_lm_yhat.RDS")
})

test_that("loop_extract_yhat behaves consistently when exploded", {
  all_yhat <- loop_extract_yhat(space_coord, time_coord,
                                simple_lm_output_exploded[[1]], simple_lm_output_exploded[[2]],
                                path_to_model = simple_lm_func,
                                use_cache = FALSE,
                                verbose = FALSE)
  expect_known_value(all_yhat, file = "simple_lm_yhat_exploded.RDS")
})

test_that("verbose = FALSE means no output to the console", {
  expect_output(loop_extract_yhat(space_coord, time_coord,
                                  simple_lm_output_exploded[[1]], simple_lm_output_exploded[[2]],
                                  path_to_model = simple_lm_func,
                                  use_cache = FALSE,
                                  verbose = FALSE),
                NA)
  expect_message(loop_extract_yhat(space_coord, time_coord,
                                   simple_lm_output_exploded[[1]], simple_lm_output_exploded[[2]],
                                   path_to_model = simple_lm_func,
                                   use_cache = FALSE,
                                   verbose = FALSE),
                 NA)
})

test_that("Throws error if colnames or labels don't match between coordinates and data_for_model", {
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

  all_fits <- simple_lm_output[[1]]
  all_data <- simple_lm_output[[2]]
  expect_error(just_coord_data(space_coord_badcolname, time_coord, all_fits, all_data),
               class = "error_bad_data_yhat")
  expect_error(just_coord_data(space_coord, time_coord_badcolname, all_fits, all_data),
               class = "error_bad_data_yhat")
  expect_error(just_coord_data(space_coord_badlabels, time_coord, all_fits, all_data),
               class = "error_bad_data_yhat")
  expect_error(just_coord_data(space_coord, time_coord_badlabels, all_fits, all_data),
               class = "error_bad_arg_value")

  all_data_unequal <- all_data
  all_data_unequal[[9]] <- all_data[[9]][-(23:27), ]
  expect_error(just_coord_data(space_coord, time_coord, all_fits, all_data_unequal),
               class = "error_bad_data_yhat")
  all_data_trimmed <- all_data
})

test_that("We can pass list or dataframe args to our extractor function and they'll be padded", {
  all_fits <- simple_lm_output[[1]]
  all_data <- simple_lm_output[[2]]
  our_extractor <- function(space_coord, time_coord, fit, data, list_of_args, other_arg) {
    yhat <- extract_yhat(space_coord, time_coord, fit, data)
    yhat[[other_arg]] <- list_of_args[[1]]
    return(yhat)
  }
  # We pass in a list, and it gets repeated
  arg1_val <- 124
  out_list_args <- list("arg1" = arg1_val, "arg2" = c(1, 2, 3), "arg3" = 3)
  out_other_arg <- "extra_col"
  all_yhats <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                    yhat_extractor_name = our_extractor,
                    path_to_model = simple_lm_func,
                    use_cache = FALSE,
                    extra_extractor_args = list(list_of_args = out_list_args,
                                                other_arg = out_other_arg))
  for (yhat in all_yhats) {
    if (identical(yhat, NA)) {
      next
    }
    expect_equal(unique(yhat$extra_col), arg1_val)
  }
  # That's still true even if it's the same length as the length of all_fits, but we do get a warning
  out_list_args <- list(8, 9, 10, 11, 12)
  names(out_list_args) <- c("arg1", "arg2", "arg3", "arg4", "arg5")
  expect_error(all_yhats <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                                yhat_extractor_name = our_extractor,
                                                path_to_model = simple_lm_func,
                                                use_cache = FALSE,
                                                verbose = FALSE,
                                                extra_extractor_args = list(list_of_args = out_list_args,
                                                                            other_arg = out_other_arg)),
                 NA)
  for (yhat in all_yhats) {
    if (identical(yhat, NA)) {
      next
    }
    expect_equal(unique(yhat$extra_col), 8)
  }
  # But if the names match up, then it's dispatched instead of repeated
  out_list_args <- list(8, 9, 10, 11, 12)
  names(out_list_args) <- names(all_fits)[!is.na(all_fits)]
  all_yhats <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                 yhat_extractor_name = our_extractor,
                                 path_to_model = simple_lm_func,
                                 use_cache = FALSE,
                                 verbose = FALSE,
                                 use_surveillance_residuals = FALSE,
                                 extra_extractor_args = list(list_of_args = out_list_args,
                                                             other_arg = out_other_arg))
  for (ind in seq_along(all_yhats)) {
    if (identical(all_yhats[[ind]], NA)) {
      next
    }
    expect_equal(unique(all_yhats[[ind]]$extra_col), out_list_args[[ind  - 7]])
  }

  # And if we pass in a data.frame, it's repeated
  out_df_arg <- as.data.frame(out_list_args)
  all_yhats <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                 yhat_extractor_name = our_extractor,
                                 path_to_model = simple_lm_func,
                                 use_cache = FALSE,
                                 verbose = FALSE,
                                 use_surveillance_residuals = FALSE,
                                 extra_extractor_args = list(list_of_args = out_df_arg,
                                                             other_arg = out_other_arg))
  for (yhat in all_yhats) {
    if (identical(yhat, NA)) {
      next
    }
    expect_equal(unique(yhat$extra_col), 8)
  }
})

test_that("Spatial dispatching works with the extra_arguments", {
  all_fits <- simple_lm_output_exploded[[1]]
  all_data <- simple_lm_output_exploded[[2]]
  our_extractor <- function(space_coord, time_coord, fit, data, list_of_args, other_arg) {
    yhat <- extract_yhat(space_coord, time_coord, fit, data)
    yhat[[other_arg]] <- list_of_args[[1]]
    return(yhat)
  }

  # We pass in a list, and it gets repeated
  # And the whole thing works equally well for gridlists.
  list_s1 <- list("a" = 1, "b" = 2, "c" = 3)
  list_s2 <- list("a" = 10, "b" = 20, "c" = 30)
  ex_list <- list("space1" = list_s1, "space2" = list_s2, "space3" = list_s2)


  out_other_arg <- "extra_col"
  all_yhats <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                 yhat_extractor_name = our_extractor,
                                 path_to_model = simple_lm_func,
                                 use_cache = FALSE,
                                 extra_extractor_args = list(list_of_args = ex_list,
                                                             other_arg = out_other_arg))
  for (ind in seq_len(nrow(all_yhats))) {
    sc_yhat <- gridcoord::gcl_as_list(all_yhats[ind, ], drop = TRUE)
    val <- ex_list[[ind]][["a"]]
    for (yhat in sc_yhat) {
      if (identical(yhat, NA)) {
        next
      }
      expect_equal(unique(yhat$extra_col), val)
    }
  }

  # And the dispatching works on exploded lists.
  # But if the names match up, then it's dispatched instead of repeated
  list_s1 <- list(8, 9, 10, 11, 12)
  names(list_s1) <- paste0("X", 8:12)
  list_s2 <- list(13, 14, 15, 16, 17)
  names(list_s2) <- paste0("X", 8:12)
  list_s3 <- list(18, 19, 20, 21, 22)
  names(list_s3) <- paste0("X", 8:12)
  ex_list <- list("space1" = list_s1, "space2" = list_s2, "space3" = list_s2)
  extra_args_gridlist <- gridcoord::gcl_gridlist(ex_list, space_coord, time_coord, trim = TRUE)



  all_yhats <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                 yhat_extractor_name = our_extractor,
                                 path_to_model = simple_lm_func,
                                 use_cache = FALSE,
                                 verbose = FALSE,
                                 use_surveillance_residuals = FALSE,
                                 extra_extractor_args = list(list_of_args = extra_args_gridlist,
                                                             other_arg = out_other_arg))

  for (ind in seq_len(nrow(all_yhats))) {
    sc_yhat <- gridcoord::gcl_as_list(all_yhats[ind, ], drop = TRUE)
    val <- gridcoord::gcl_as_list(extra_args_gridlist[ind, ], drop = TRUE)
    for (c_ind in seq_along(sc_yhat)) {
      if (identical(sc_yhat[[c_ind]], NA)) {
        next
      }
      nm <- names(sc_yhat)[[c_ind]]
      expect_equal(unique(sc_yhat[[c_ind]]$extra_col), val[[nm]])
    }
  }

})




test_that("Still works if we only have one model fit", {
  all_fits <- simple_lm_output[[1]][9] #Note - needs to be a list
  all_data <- simple_lm_output[[2]][9]
  all_yhat <- just_coord_data(space_coord, time_coord, all_fits, all_data)
  expect_equal(class(all_yhat), "list")
  expect_true("data.frame" %in% class(all_yhat[[9]]))
})

test_that("Still works even if the frames in all_data are only one row", {
  all_fits <- simple_lm_output[[1]]
  short_data <- lapply(simple_lm_output[[2]],
                       function(x) {
                         if (is.data.frame(x)) {
                           return(x[1, , drop = FALSE])
                         }
                         x
                       })
  expect_error(all_yhat <- just_coord_data(space_coord, time_coord, all_fits, short_data),
               NA)
  for (yhat in all_yhat) {
    if (identical(yhat, NA)) next
    expect_true("data.frame" %in% class(yhat))
    expect_equal(nrow(yhat), 1)
  }
})

test_that("Throws an error if fits and data don't agree", {
  # Error if the lengths don't match
  all_fits <- simple_lm_output[[1]]
  all_data <- simple_lm_output[[2]][1:3]
  expect_error(just_coord_data(space_coord, time_coord, all_fits, all_data),
               class = "error_bad_arg_value")
  # Error if one doesn't have names
  all_data <- simple_lm_output[[2]]
  names(all_data) <- NULL
  expect_error(just_coord_data(space_coord, time_coord, all_fits, all_data),
               class = "error_bad_arg_value")
  # Error if the names don't line up with time_coord
  names(all_data) <- paste0("badname", names(all_fits))
  expect_error(just_coord_data(space_coord, time_coord, all_fits, all_data),
               class = "error_bad_arg_value")
})

test_that("Throws an error if fits and data don't agree (exploded list)", {
  all_fits <- simple_lm_output_exploded[[1]]
  all_data <- simple_lm_output_exploded[[2]]
  all_data <- all_data[, 1:4]
  expect_error(just_coord_data(space_coord, time_coord, all_fits, all_data),
               class = "error_bad_arg_value")
  # Error if one doesn't have names (Not actually possible now)

  # # Error if the names don't line up with time_coord
  # colnames(all_data) <- paste0("badname", colnames(all_fits))
  # expect_error(just_coord_data(space_coord, time_coord, all_fits, all_data),
  #              class = "error_bad_arg_value")

})

test_that("What we get out matches what we put in", {
  # Matches for extract, multi-variate
  all_fits <- simple_lm_output[[1]]
  all_data <- simple_lm_output[[2]]
  all_yhat <- just_coord_data(space_coord, time_coord, all_fits, all_data)
  expect_equal(class(all_yhat), class(all_fits))
  expect_equal(length(all_yhat), length(all_fits))

  # And for sample, multi-variate
  all_yhat <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                yhat_extractor_name = "sample",
                                path_to_model = simple_lm_func,
                                use_cache = FALSE,
                                n_samples = 10,
                                verbose = FALSE)
  expect_equal(class(all_yhat), class(all_fits))
  expect_equal(length(all_yhat), length(all_fits))

  # And for extract, exploded
  all_fits <- simple_lm_output_exploded[[1]]
  all_data <- simple_lm_output_exploded[[2]]
  all_yhat <- just_coord_data(space_coord, time_coord, all_fits, all_data)
  expect_equal(class(all_yhat), class(all_fits))
  expect_equal(length(all_yhat), length(all_fits))
  for (ind in seq_along(all_yhat)) {
    expect_equal(class(all_yhat[[ind]]), class(all_fits[[ind]]))
    expect_equal(length(all_yhat[[ind]]), length(all_fits[[ind]]))
  }

  # And finally for sample, exploded
  all_yhat <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                yhat_extractor_name = "sample",
                                path_to_model = simple_lm_func,
                                use_cache = FALSE,
                                n_samples = 10,
                                verbose = TRUE)
  expect_equal(class(all_yhat), class(all_fits))
  expect_equal(length(all_yhat), length(all_fits))
  for (ind in seq_along(all_yhat)) {
    expect_equal(class(all_yhat[[ind]]), class(all_fits[[ind]]))
    expect_equal(length(all_yhat[[ind]]), length(all_fits[[ind]]))
  }
})

# CUrrently here

test_that("We can cache things appropriately", {
  all_fits <- simple_lm_output[[1]]
  all_data <- simple_lm_output[[2]]
  cache_subdir <- substring(tempfile(tmpdir = ""), 2)
  all_yhat1 <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                 path_to_model = simple_lm_func, verbose = FALSE,
                                 use_cache = TRUE)
  for (yhat in all_yhat1) {
    if (identical(yhat, NA)) next
    expect_false(attr(yhat, ".used_cache"))
  }
  all_yhat2 <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                 path_to_model = simple_lm_func, verbose = FALSE,
                                 use_cache = TRUE)
  for (yhat in all_yhat2) {
    if (identical(yhat, NA)) next
    expect_true(attr(yhat, ".used_cache"))
  }

  #And this is true even if path_to_model == NULL
  all_yhat1 <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                path_to_model = NULL, verbose = FALSE,
                                use_cache = TRUE)
  for (yhat in all_yhat1) {
    if (identical(yhat, NA)) next
    expect_false(attr(yhat, ".used_cache"))
  }
  all_yhat2 <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                 path_to_model = NULL, verbose = FALSE,
                                 use_cache = TRUE)
  for (yhat in all_yhat2) {
    if (identical(yhat, NA)) next
    expect_true(attr(yhat, ".used_cache"))
  }

  # And it's true if we have an exploded model (i.e., spatial dispatch works on NULL args)
  all_fits <- simple_lm_output_exploded[[1]]
  all_data <- simple_lm_output_exploded[[2]]
  cache_subdir <- substring(tempfile(tmpdir = ""), 2)
  all_yhat1 <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                 path_to_model = simple_lm_func, verbose = FALSE,
                                 use_cache = TRUE)
  for (space_yhat in all_yhat1) {
    for (yhat in space_yhat) {
      if (identical(yhat, NA)) next
      expect_false(attr(yhat, ".used_cache"))
    }
  }
  all_yhat2 <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                 path_to_model = simple_lm_func, verbose = FALSE,
                                 use_cache = TRUE)
  for (space_yhat in all_yhat2) {
    for (yhat in space_yhat) {
      if (identical(yhat, NA)) next
      expect_true(attr(yhat, ".used_cache"))
    }
  }


  #And this is true even if path_to_model == NULL
  all_yhat1 <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                 path_to_model = NULL, verbose = FALSE,
                                 use_cache = TRUE)
  for (space_yhat in all_yhat1) {
    for (yhat in space_yhat) {
      if (identical(yhat, NA)) next
      expect_false(attr(yhat, ".used_cache"))
    }
  }
  all_yhat2 <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                 path_to_model = NULL, verbose = FALSE,
                                 use_cache = TRUE)
  for (space_yhat in all_yhat2) {
    for (yhat in space_yhat) {
      if (identical(yhat, NA)) next
      expect_true(attr(yhat, ".used_cache"))
    }
  }
  unlink("cache_yhat", recursive = TRUE)
})

test_that("We can supply our own extractor functions", {
  all_fits <- simple_lm_output[[1]]
  all_data <- simple_lm_output[[2]]

  all_yhat <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                yhat_extractor_name = "model_extractor.R",
                                use_cache = FALSE, verbose = FALSE)
  all_yhat_orig <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                     yhat_extractor_name = "extract",
                                     use_cache = FALSE, verbose = FALSE)
  expect_equal(lapply(all_yhat, dim),
               lapply(all_yhat_orig, dim))

  expect_equal(all_yhat$X11$yhat/2, all_yhat_orig$X11$yhat)

  all_yhat_v2 <- loop_extract_yhat(space_coord, time_coord, all_fits, all_data,
                                   yhat_extractor_name = "model_extractor2.R",
                                   use_cache = FALSE, verbose = FALSE,
                                   extra_extractor_args = list(scaling = 3))
  q <- loop_over(space_coord, time_coord, all_yhat_v2, collapse_yhat,
            extra_args = list(space_coord = space_coord, time_coord = time_coord))
  expect_equal(q, all_yhat)
})



options(op)
