#' @keywords internal
collapse_scan_bayes <- function(list_of_scans, zones) {
  alarm_res <- list_of_scans[[1]]

  priors <- lapply(list_of_scans, pluck_out, "priors") %>%
    list_transpose() %>%
    lapply(stack_and_average)


  posteriors <- lapply(list_of_scans, pluck_out, "posteriors") %>%
    list_transpose()
  posteriors$window_posteriors <- lapply(posteriors$window_posteriors,
                                         function(x) {
                                           dplyr::arrange(x, .data$zone, .data$duration)
                                         })
  posteriors <- lapply(posteriors, stack_and_average)

  if ("observed" %in% names(alarm_res)) {
    observed <- lapply(list_of_scans, pluck_out, "observed")
    observed <- lapply(observed,
                       function(x) {
                         dplyr::arrange(x, .data$zone, .data$duration)
                       })
    observed <- stack_and_average(observed) %>%
      dplyr::arrange(dplyr::desc(.data$action_level))
    alarm_res$observed <- observed
  } else {
    alarm_res$observed <- posteriors$window_posteriors
  }

  marginal_data_prob <- lapply(list_of_scans, pluck_out, "marginal_data_prob") %>%
    stack_and_average()

  alarm_res$priors <- priors
  alarm_res$posteriors <- posteriors
  alarm_res$marginal_data_prob <- marginal_data_prob

  # Then all that's left is calculating the MLC
  mlc_ind <- which.max(alarm_res$observed$action_level)
  mlc_row <- alarm_res$observed[mlc_ind, , drop = FALSE]
  MLC <- list("zone" = mlc_row$zone,
              "duration" = mlc_row$duration,
              "log_posterior" = mlc_row$log_posterior,
              "log_bayes_factor" = mlc_row$log_bayes_factor,
              "posterior" = exp(mlc_row$log_posterior),
              "locations" = zones[[mlc_row$zone]],
              "action_level" = mlc_row$log_posterior)
  alarm_res$MLC <- MLC
  return(alarm_res)
}

# collapse_items <- function(list_of_stuff)

