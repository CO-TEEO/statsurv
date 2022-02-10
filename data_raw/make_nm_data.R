library(scanstatistics)
data(NM_popcas)

NM_data <- NM_popcas %>%
  dplyr::mutate(id_time = year - min(year) + 1,
                id_space = as.numeric(factor(county)),
                .before = 1) %>%
  dplyr::mutate(baseline_est = population * mean(count) / mean(population))

usethis::use_data(NM_data, overwrite = TRUE)
