#' Data frames including information on space and time
#'
#' @param spacetime_data A data frame or a data frame extension (e.g., a tibble) that includes
#'   columns `id_space` and `id_time` that are each a set of consecutive integers. The column
#'   `id_time` identifies the time point associated with each row, with lower numbers being earlier
#'   and higher numbers being later. The column `id_space` identifies the spatial location or area
#'   associated with each row, but no meaning or order is ascribed to the values.
#' @name spacetime_data
NULL
#> NULL

validate_spacetime_data <- function(df) {
  # This is basically the definition of the spacetime dataframe.
  # Requirements:
  # Has the columns id_time and id_space
  # Each is integers or equivalent
  # Each has the values seq(min, max, by = 1)
  if (!"id_time" %in% colnames(df) || !"id_space" %in% colnames(df)) {
    stop_subclass("dataframes used in statsurv must include the columns 'id_time' and 'id_space'",
                  .subclass = "error_bad_spacetimedata")
  }

  if (!rlang::is_integerish(df$id_time, finite = TRUE)) {
    stop_subclass("The column 'id_time' must be made of whole numbers with no gaps or skips",
                  .subclass = "error_bad_spacetimedata")

  }
  if (!rlang::is_integerish(df$id_space, finite = TRUE)) {
    stop_subclass("The column 'id_space' must be made of whole numbers with no gaps or skips",
                  .subclass = "error_bad_spacetimedata")

  }
  time_uniq_val <- sort(unique(df$id_time))
  if (!all.equal(time_uniq_val, seq(min(df$id_time), max(df$id_time), by = 1))) {
    stop_subclass("The column 'id_time' must be made of whole numbers with no gaps or skips",
                  .subclass = "error_bad_spacetimedata")
  }

  space_uniq_val <- sort(unique(df$id_space))
  if (!all.equal(space_uniq_val, seq(min(df$id_space), max(df$id_space), by = 1))) {
    stop_subclass("The column 'id_space' must be made of whole numbers with no gaps or skips",
                  .subclass = "error_bad_spacetimedata")
  }
  return(invisible(df))
}
