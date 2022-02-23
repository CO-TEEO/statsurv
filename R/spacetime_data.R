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


#' Check that a data frame is formatted as a valid spacetime data frame
#'
#' The `statsurv` package represents spacetime data as ordinary data frames that follow some
#' specific conventions. These data frames must contain the columns `id_space` and `id_time`, that
#' are each a set of consecutive integers. The column `id_time` identifies the time point associated
#' with each row of the data frame, with lower numbers being earlier and higher numbers being later.
#' The column `id_space` identifies the spatial location or area associated with each row. The
#' specific values of `id_space` are not assumed to have any meaning or order beyond identification.
#'
#' @details To be a valid spacetime data frame, all of the following must be true:
#' \itemize{
#'   \item The data frame must contain the columns `id_time` and `id_space`.
#'   \item The entires in each of those columns must be consecutive integers
#'   \item All combinations of `id_time` and `id_space` must be associated with at most 1 row.
#'}
#'
#' @param df The data frame to validate
#'
#' @return Throws an error if the data frame is not a valid spacetime data frame. Otherwise, returns
#'   `df`, invisibly.
#' @export
#' @md
#'
#' @examples
#' valid_data <- data.frame(id_space = c(1, 2, 1, 2),
#' id_time = c(1, 1, 2, 2),
#' y = c(0.4, 0.5, 0.7, 0.8))
#'
#' validate_spacetime_data(valid_data)
#'
#' \dontrun{
#'   invalid_data <- data.frame(id_space = c(1, 2.5, 1, 2.5),
#'                              id_time = c(1, 1, 2, 2),
#'                              y = c(-1, -2, -0.5, -8))
#'   #Gives an error
#'   validate_spacetime_data(invalid_data)
#' }
validate_spacetime_data <- function(df) {
  # This is basically the definition of the spacetime dataframe.
  # Requirements:
  # Has the columns id_time and id_space
  # Each is integers or equivalent
  # Each has the values seq(min, max, by = 1)
  # Rows much be uniquely defined by id_space and id_time? Does that break anything?
  if (!"id_time" %in% colnames(df) || !"id_space" %in% colnames(df)) {
    rlang::abort("dataframes used in statsurv must include the columns 'id_time' and 'id_space'",
                  class = "error_bad_spacetimedata")
  }

  if (!rlang::is_integerish(df$id_time, finite = TRUE)) {
    rlang::abort("The column 'id_time' must be made of whole numbers with no gaps or skips",
                 class = "error_bad_spacetimedata")

  }
  if (!rlang::is_integerish(df$id_space, finite = TRUE)) {
    rlang::abort("The column 'id_space' must be made of whole numbers with no gaps or skips",
                 class = "error_bad_spacetimedata")

  }
  time_uniq_val <- sort(unique(df$id_time))
  if (!all.equal(time_uniq_val, seq(min(df$id_time), max(df$id_time), by = 1))) {
    rlang::abort("The column 'id_time' must be made of whole numbers with no gaps or skips",
                 class = "error_bad_spacetimedata")
  }

  space_uniq_val <- sort(unique(df$id_space))
  if (!all.equal(space_uniq_val, seq(min(df$id_space), max(df$id_space), by = 1))) {
    rlang::abort("The column 'id_space' must be made of whole numbers with no gaps or skips",
                 class = "error_bad_spacetimedata")
  }

  return(invisible(df))
}

#' Pad a spacetime data frame
#'
#' @param df A data frame or a data frame extension (e.g., a tibble) that includes
#'   columns `id_space` and `id_time` that are each integers. Unlike \code{\link{spacetime_data}},
#'   there may be gaps or skips in the entries in these columns.
#'
#' @return A data frame padded with extra rows to be a valid spacetime data frame. After padding,
#'   all  combinations of `id_space` and `id_time` are associated with at least one row, and there
#'   are no skipped values in those two columns. Added rows contain `NA` for all columns other than
#'   `id_space` and `id_time`.
#' @export
#' @md
#'
#' @seealso \code{\link{spacetime_data}} \code{\link{validate_spacetime_data}}
#'
#' @examples
#' df <- data.frame(id_space = c(1, 2),
#'                  id_time = c(1, 3),
#'                  y = c(1, 3))
#' padded_data <- pad_spacetime_data(df)
#' validate_spacetime_data(padded_data)
pad_spacetime_data <- function(df) {
  if (!"id_time" %in% colnames(df) || !"id_space" %in% colnames(df)) {
    rlang::abort("dataframes used in statsurv must include the columns 'id_time' and 'id_space'",
                  class = "error_bad_spacetimedata")
  }

  if (!rlang::is_integerish(df$id_time, finite = TRUE)) {
    rlang::abort("The column 'id_time' must be made of whole numbers with no gaps or skips",
                  class = "error_bad_spacetimedata")

  }
  if (!rlang::is_integerish(df$id_space, finite = TRUE)) {
    rlang::abort("The column 'id_space' must be made of whole numbers with no gaps or skips",
                  class = "error_bad_spacetimedata")

  }

  coord_df <- expand.grid(id_space = seq(min(df$id_space), max(df$id_space)),
                          id_time = seq(min(df$id_time), max(df$id_time)))

  dplyr::full_join(df, coord_df, by = c("id_space", "id_time")) %>%
    dplyr::arrange(.data$id_space, .data$id_time)
}
