

is_multiariate <- function(space_coord, df = NULL) {
  # df can either be null or a coordinate
  check_type(df, c("data.frame", "NULL"))
  if (is.null(df)) {
    sc_labels <- length(gridcoord::gc_get_labels(space_coord))
    return(sc_labels > 1)
  }

  sc_name <- gridcoord::gc_get_name(space_coord)
  arity <- length(unique(df[[sc_name]]))
  return(arity > 1)
}

# Turn a gridlist into a dataframe.
collapse_if_exploded <- function(space_coord, time_coord, x) {
  if (is_type(x, "gridlist")) {
    x <- gridcoord::gcl_collapse(x,
                                 collapse_by = "space",
                                 return_gridlist = TRUE,
                                 simplify = TRUE) %>%
      extract_gcl_row(1)
    x <- lapply(x,
                function(y) {
                  if (nrow(y) == 0) {
                    NA
                  } else {
                    y
                  }
                })

  }
  return(x)
}
