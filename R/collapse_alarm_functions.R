#' @keywords internal
collapse_alarm_functions <- function(list_of_alarm_functions, alarm_function_name) {
  # Internal function - if the user has a multi-column baseline to calculate a scan-statistic on,
  # they can use standardized_alarm_multicol
  #Note that this assumes that there is an action_level column
  # And a zone_info entry in each alarm function


  alarm_res <- list_of_alarm_functions[[1]]
  alarm_type <- attr(alarm_res, "alarm_type")

  if (alarm_function_name == "scan_bayes_negbin") {
    zones <- ensure_zones(alarm_res$zone_info)
    alarm_res <- collapse_scan_bayes(list_of_alarm_functions, zones)
    return(alarm_res)
  }

  if (alarm_type == "scan") {
    zones <- ensure_zones(alarm_res$zone_info)
    observed_list <- lapply(list_of_alarm_functions, pluck_out, "observed")
    observed_list <- lapply(observed_list,
                            function(x) dplyr::arrange(x, .data$zone, .data$duration))
    observed <- stack_and_average(observed_list) %>%
      dplyr::arrange(dplyr::desc(.data$score))


    replicates_list <- lapply(list_of_alarm_functions, pluck_out, "replicates")
    replicates <- do.call(rbind, replicates_list)

    # 5. A lot of annoying boilerplate turning out results back into a scanstatistic
    alarm_res$observed <- observed
    ind <- which.max(alarm_res$observed$score)
    mlc_row <- alarm_res$observed[ind, , drop = FALSE]
    mlc_row$zone_number <- mlc_row$zone
    mlc_row$zone <- NULL
    MLC <- as.list(mlc_row)
    MLC$locations <- zones[[mlc_row$zone_number]]

    MC_pvalue <- mc_pvalue(MLC$action_level, replicates$action_level)
    Gumbel_pvalue <- NULL


    alarm_res$MLC <- MLC
    alarm_res$replicates <- replicates
    alarm_res$MC_pvalue <- MC_pvalue
    alarm_res$Gumbel_pvalue <- Gumbel_pvalue
    alarm_res$n_mcsim <- alarm_res$n_mcsim * length(list_of_alarm_functions)
    return(alarm_res)
  }

  if (alarm_type == "parallel") {
    # We're using a 3-dimensional array to do this.
    m <- vapply(list_of_alarm_functions,
                function(x) !is.null(x),
                logical(1))
    list_of_alarm_functions <- list_of_alarm_functions[m]

    collapsed_parallel <- stack_and_average(list_of_alarm_functions)
    attr(collapsed_parallel, "alarm_type") <- "parallel"
    return(collapsed_parallel)
  }
}
