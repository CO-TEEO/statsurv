# For this, we're going to want...what?
# I think we just want...let's make a 3x3 grid

# Ok, let's make a toy system thats a 1x2 spatial and has 2 time points - I think that's a simple
# enough toy system that I can check it by hand
x_coord <- c(1, 2)
y_coord <- c(1, 1)
geo <- matrix(c(x_coord, y_coord), ncol = 2, byrow = FALSE)
zones_sm <- geo %>%
  scanstatistics::coords_to_knn(k = 1) %>%
  scanstatistics::knn_zones()
key_matrix_sm <- zones_to_key_matrix(zones_sm)

spacetime_data_sm <-
  data.frame(id_space = c(1, 2, 1, 2),
             id_time = c(1, 2),
             cases = c(2, 2, 3, 5),
             .fitted = 2)
wide_cases_sm <- matrix(data = c(2, 2, 3, 5), nrow = 2, byrow = TRUE)
wide_baseline_sm <- matrix(2, nrow = 2, ncol = 2)
outbreak_sp_sm <- c(2)
outbreak_tm_sm <- c(2)

# Then we'll make a larger one, that's a 3x3 grid with an outbreak in 1-2-4-5
# And 4 time points, with an outbreak in the most recent 2
x_coord <- rep(c(1:3), 3)
y_coord <- rep(c(1:3), each =3)
geo <- matrix(c(x_coord, y_coord), ncol = 2, byrow = FALSE)
zones_lg <- geo %>%
  scanstatistics::coords_to_knn(k = 4) %>%
  scanstatistics::knn_zones()
key_matrix_lg <- zones_to_key_matrix(zones_lg)

spacetime_data_lg <- data.frame(id_space = rep(1:9, 4),
                                id_time = rep(1:4, each = 9)) %>%
  dplyr::mutate(cases = dplyr::case_when(id_space %in% c(1, 2, 4, 5) & id_time %in% c(3,4) ~ 5,
                                         id_space %in% c(8,9) & id_time %in% c(3, 4) ~ 3,
                                         TRUE ~ 2),
                .fitted = 2)
outbreak_sp_lg <- c(1, 2, 4, 5)
outbreak_tm_lg <- c(3, 4)
wide_cases_lg <- matrix(2, nrow = 4, ncol = 9)
wide_cases_lg[outbreak_tm_lg, outbreak_sp_lg] <- 5
wide_cases_lg[c(3, 4), c(8, 9)] <- 3

wide_baseline_lg <- matrix(2, nrow = 4, ncol = 9)
