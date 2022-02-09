
#' Merge rows from each group into a list
#'
#' `collapse_all()` takes a data frame and for each column, combines all the entries for each group
#' into a list. Other parameters control whether data frames should be merged into a single longer
#' data frame, whether only unique values should be reported, or whether columns should be
#' transformed back into atomic columns where possible.
#'
#'
#' While `collapse_all` can be used on any data frame, it's designed around data frames that having
#' a sequential series of model fits. Each model might be associated with some parameters, such as
#' what date and location it corresponds to, some data, and some predictions. If you have a separate
#' model for each location, it's natural to want to combine the predictions from each location into
#' a larger dataset that you can look at together. `collapse_all` simplifies the process of
#' combining the data and predictions from each model fit into a single data frame. `collapse_all`
#' works with grouping variables, so if you have model fits for multiple locations from multiple
#' different models, it's easy to combine the predictions from each model separately.
#'
#' One way to think about `collapse_all` is that it's halfway between \code{\link[dplyr]{summarize}}
#' and \code{\link[tidyr]{nest}}. Like `summarize`, it combines all the rows for each group into a
#' single row. Like `nest`, it stores all the data (unlike `unique_only = TRUE`), instead of
#' reporting a single summary value for each group. Unlike `nest` though, `collapse_all` works on
#' each column individually, and it stores the data as a list instead of as a data frame.
#'
#' In fact, `collapse_all` is a wrapper around `summarize` (not `nest`), and a call to
#' `collapse_all` can always be replaced with a call to `summarize`, with assistance from the helper
#' functions \code{\link{collapse}} and \code{\link{unlist_scalars}}:
#' \preformatted{
#' df %>%
#'    collapse_all()
#' }
#' is equivalent to
#' \preformatted{
#' df %>%
#'   summarize(across(everything()), .fns = collapse, combined_dfs = TRUE) %>%
#'   unlist_scalars()
#' }
#'
#' @param df A data frame or data frame extension. Can be grouped.
#' @param combine_dfs Logical. Whether columns containtaining data frames should be combined into a
#'   single longer data frame (`combine_dfs = TRUE`) or left as a list of data frames (`combine_dfs
#'   = FALSE`)
#' @param unique_only Logical. Whether the entries in a list should contain the values from all
#'   rows, including repeats (`unique_only = FALSE`) or if only unique values should be reported
#'   (`unique_only = TRUE`). Does not apply to data frames if `combine_dfs = TRUE`.
#' @param unlist_scalars Logical. After columns are reduced to a single row, should columns
#'   containing scalar values be converted from list columns into regular columns.
#' @inheritParams dplyr::summarize
#'
#' @return An object of the same type as `df`, with one row per group in the original data frame.
#'   Columns other than grouping variables are transformed into list columns. If `unlist_scalars =
#'   TRUE`, some of the list columns may be conveted back into regular columns.
#' @export
#' @md
#'
#' @examples
#' mtcars_nested <- mtcars %>%
#'   group_by(cyl) %>%
#'   nest()
#'
#' # Generate two models for mpg with different predicting variables
#' model_fits <- tidyr::crossing(mtcars_nested, model_var = c("wt", "hp")) %>%
#'   rowwise() %>%
#'   mutate(model = list(lm(paste0("mpg ~ ", model_var), data = data)),
#'          predictions = list(broom::augment(model)))
#'
#' # Combine the results from each model separately
#' model_fits %>%
#'   group_by(model_var) %>%
#'   collapse_all()
collapse_all <- function(df, combine_dfs = TRUE, unique_only = FALSE, unlist_scalars = TRUE, .groups = NULL) {
  df <- df %>%
    dplyr::summarize(dplyr::across(everything(), .fns = collapse,
                     combine_dfs = combine_dfs, unique_only = unique_only),
              .groups = .groups)
  if (unlist_scalars) {
    df <- unlist_scalars(df)
  }
  return(df)
}

#' Merge entries into a list of length 1
#'
#' Take a vector or list and combine the entries into a list of length 1.
#'
#' `collapse()` is a helper function used by \code{\link{collapse_all}}, but can also be used in
#' conjunction with \code{\link[dplyr]{summarize}} to reduce the number of rows in a data frame
#' without losing information.
#'
#' @param vec An atomic vector or a list.
#' @inheritParams collapse_all
#'
#' @return A list of length one. If `combine_dfs = FALSE, unique_only = FALSE`, then `collapse()`
#'   returns a `list(vec)`. If `combine_dfs = TRUE`, and `vec` is a list of data frames, then
#'   `collapse()` returns `list(bind_rows(vec))`. If `unique_only = TRUE`, then duplicate entries of
#'   `vec` are removed.
#' @export
#'
#' @examples
#' vec <- c(1, 2, 3, 3, 4)
#' collapse(vec)
#'
#' collapse(vec, unique_only = TRUE)
#'
#' df1 <- data.frame(x = c(1, 2, 3), y = c("A", "B", "C"))
#' df2 <- data.farme(x = c(4, 5), y= c("D", "E"))
#' collapse(list(df1, df2), combine_dfs = TRUE)
#'
#' # collapse can be used together with summarize:
#' mtcars_nested <- mtcars %>%
#'   group_by(cyl) %>%
#'   nest()
#'
#' # Generate two models for mpg with different predicting variables
#' model_fits <- tidyr::crossing(mtcars_nested, model_var = c("wt", "hp")) %>%
#'   rowwise() %>%
#'   mutate(model = list(lm(paste0("mpg ~ ", model_var), data = data)),
#'          predictions = list(broom::augment(model)))
#'
#' # Combine the results from each model separately
#' model_fits %>%
#'   group_by(model_var) %>%
#'   summarize(models = collapse(model),
#'             predictions = collapse(predictions, combine_dfs = TRUE))
collapse <- function(vec, combine_dfs = TRUE, unique_only = FALSE) {



  if (combine_dfs) {
    is_df <- purrr::map_lgl(vec, is.data.frame)
    if (all(is_df)) {
      return(list(dplyr::bind_rows(vec)))
    }
  }

  if (unique_only) {
    vec <- unique(vec)
  }

  ensure_list_of_one(vec)
}

#' Convert list columns back to regular columns where possible
#'
#' Given a data frame, convert any list columns containing only atomic scalars into regular columns.
#' Any grouping structure of the data frame is preserved.
#' @inheritParams collapse_all
#'
#' @return A data frame with the same column names and grouping structure as the input.
#' @export
#'
#' @examples
#' # Create a data frame with some list columns
#' df <- tibble::tibble(x = c(1, 2, 3),
#'                      y = list("A", "B", "C"),
#'                      z = list(14, 201, c(99, 45)))
#'
#' # Converts y into a regular column, leaves z as a list column
#' unlist_scalars(df)
unlist_scalars <- function(df) {
  is_scalar <- function(x) {
    length(x) == 1 && !rlang::is_bare_list(x)
  }
  for (ii in seq_len(ncol(df))) {
    col <- df[[ii]]
    if (all(purrr::map_lgl(col, is_scalar))) {
      df[[ii]] <- tryCatch(vctrs::vec_c(!!!col),
                           error = function(e) {col})
    }
  }
  df
}



