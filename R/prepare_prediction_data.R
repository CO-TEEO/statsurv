#' Prepare data for prediction in statistical surveillance
#'
#' @inheritParams spacetime_data
#' @param outcome_cols \code{<\link[tidyr]{tidyr_tidy_select }>} Outcome columns that should be
#'   removed or NA'd out of the training data. Follows the `tidy-select` principles of the tidyverse.
#' @param split_id The value of `id_time` that marks the beginning of the prediction data. All
#'   entries in the outcome columns associated with an `id_time` that is greater than or equal to
#'   `split_id` will be removed or NA'd out of the training data.
#' @param prep_strategy Whether the values should be converted to NA (`prep_strategy = "NA"`) or if
#'   those rows should be removed (`prep_strategy = "truncate"`)
#'
#' @return A data frame with the selected values converted to NA or removed, as designated by
#'   `prep_strategy`
#' @export
#' @md
#'
#' @examples
#' x = rnorm(100)
#' spacetime_data <- data.frame(id_space = rep(1:10, each = 10),
#'                              id_time = rep(1:10, 10),
#'                              x = x,
#'                              y = 2.04 * x + 1.23,
#'                              y2 = (2.04 * x + 1.23)^2)
#'
#' # NA-out a single column
#' prepare_prediction_data(spacetime_data, y, split_id = 9)
#'
#' # NA-out multiple columns
#' prepare_prediction_data(spacetime_data, c(y, y2), split_id = 9)
#'
#' # We can use dplyr selection helpers
#' prepare_prediction_data(spacetime_data, dplyr::starts_with("y"), split_id = 9)
#'
#' # Remove all rows above split_id
#' prepare_prediction_data(spacetime_data, y, split_id = 9, prep_strategy = "truncate")
prepare_prediction_data <- function(spacetime_data, outcome_cols, split_id,
                                    prep_strategy = c("NA", "truncate")) {

  # Arg checks
  validate_spacetime_data(spacetime_data)
  stopifnot(rlang::is_scalar_integerish(split_id))
  # stopifnot(rlang::is_character(outcome_cols))
  prep_strategy <- match.arg(prep_strategy, several.ok = FALSE)


  # stopifnot(all(outcome_cols %in% colnames(spacetime_data)))
  stopifnot(split_id >= min(spacetime_data$id_time),
            split_id <= max(spacetime_data$id_time))

  # Actual code
  mask <- spacetime_data$id_time >= split_id

  if (prep_strategy == "NA") {
    prep_f <- function(vec, id_time, split_id) {
      # Need to do it this complicated way to handle data frame columns
      ndim <- length(dim(vec))
      if (ndim <= 1) { #Most vectors return NULL, which has a length of 0
        vec[mask] <- NA
      } else if (ndim == 2) {
        vec[mask, ] <- NA
      } else { #I don't think you can have 3-dimensional arrays in a data.frame, but just in case
        browser()
      }
      return(vec)
    }
    spacetime_data <- dplyr::mutate(spacetime_data, dplyr::across({{outcome_cols}}, .fns = prep_f,
                                                id_time = .data$id_time,
                                                split_id = .env$split_id))
  } else if (prep_strategy == "truncate") {
    spacetime_data <- spacetime_data[!mask, ]
  } else {
    stop("Invalid value for 'prep_strategy'")
  }
  return(spacetime_data)

}
