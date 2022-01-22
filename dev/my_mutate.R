my_mutate <- function (.data, ..., .keep = c("all", "used", "unused",
                                "none"), .before = NULL, .after = NULL)
{
  browser()
  keep <- arg_match(.keep)
  cols <- dplyr:::mutate_cols(.data, ..., caller_env = caller_env())
  out <- dplyr:::dplyr_col_modify(.data, cols)
  .before <- enquo(.before)
  .after <- enquo(.after)
  if (!quo_is_null(.before) || !quo_is_null(.after)) {
    new <- setdiff(names(cols), names(.data))
    out <- relocate(out, !!new, .before = !!.before, .after = !!.after)
  }
  if (keep == "all") {
    out
  }
  else if (keep == "unused") {
    used <- attr(cols, "used")
    unused <- names(used)[!used]
    keep <- intersect(names(out), c(group_vars(.data), unused,
                                    names(cols)))
    dplyr_col_select(out, keep)
  }
  else if (keep == "used") {
    used <- attr(cols, "used")
    used <- names(used)[used]
    keep <- intersect(names(out), c(group_vars(.data), used,
                                    names(cols)))
    dplyr:::dplyr_col_select(out, keep)
  }
  else if (keep == "none") {
    keep <- c(setdiff(group_vars(.data), names(cols)), intersect(names(cols),
                                                                 names(out)))
    dplyr_col_select(out, keep)
  }
}
