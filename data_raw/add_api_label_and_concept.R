library(magrittr)
bare_ref <- read.csv(file.path("data_raw",
                               "census_api_reference_empty.csv"),
                     stringsAsFactors = FALSE) %>%
  tibble::as_tibble()

b_ref <- bare_ref %>%
  dplyr::filter(startsWith(api_var, "B"))
s_ref <- bare_ref %>%
  dplyr::filter(startsWith(api_var, "S"))

b_vars <- tidycensus::load_variables(2017, "acs5", cache = TRUE)
api_b_ref <- dplyr::left_join(b_ref, b_vars, by = c("api_var" = "name"))

s_vars <- tidycensus::load_variables(2017, "acs5/subject", cache = TRUE)
api_s_ref <- dplyr::left_join(s_ref, s_vars, by = c("api_var" = "name"))

api_ref <- rbind(api_b_ref, api_s_ref) %>%
  dplyr::select(api_var, label, concept, human_name, notes)

out_name <- file.path("data_raw",
                      "census_api_reference.csv")
if (!file.exists(out_name)) {
  write.csv(api_ref,
            out_name,
            row.names = FALSE)
} else {
  message("File already exists - nothing written")
}
