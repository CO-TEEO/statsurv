set.seed(65616181)

# We're going to set up a very basic model and then try to break it
space_coord <- data.frame(space_label = c("space1", "space2", "space3"),
                          stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()



start_times <- 1:12
fin_times <- start_times + 1L
labels <- paste0("X", start_times) #sapply(start_times + 64, intToUtf8)
time_coord <- data.frame(time_labels = labels,
                         time = start_times,
                         fin_time = fin_times,
                         stringsAsFactors = FALSE) %>%
  gridcoord::gc_gridcoord()
data_for_model <- gridcoord::gc_expand(time_coord, space_coord)
data_for_model$y <- data_for_model$time * 1.05 + rnorm(n = nrow(data_for_model))


spacetime_data <- data_for_model %>%
  dplyr::mutate(id_space = as.numeric(factor(space_label)),
                id_time = time) %>%
  tibble::as_tibble() %>%
  dplyr::select(id_space, id_time, time, fin_time, y)

simple_lm_func <- function(data_for_model) {
  fit <- lm(y ~ time,
            data = data_for_model)
  return(fit)
}

windowed_data <- window_idtime(spacetime_data,
                                  min_train = 5,
                                  max_train = 7,
                                  n_predict = 1,
                                  model_arity = "multi")


df_after_loop_model <- windowed_data %>%
  rowmute(data_for_model = prepare_prediction_data(curr_data, id_time, split_id, )) %>%
  rowmute(fit = simple_lm_func(data_for_model))

library(generics)
library(broom)
df_after_augment <- df_after_loop_model %>%
  rowmute(aug_data = generics::augment(fit, newdata = curr_data))
# Not sure how much we'll be able to use augment. It seems like it's pretty finicky and might not work
# with forecast or fable.

# Ok, now I need to convert to surveillance. Maybe?
# I'm not actually sure that I need to.
# I think generally we'd say not?
# But it would be good to have the option.
# And that's one where we'd want to clarify that it's not rowwise (because we have to combine them)


windowed_data_ex <- window_idtime(spacetime_data,
                                  min_train = 5,
                                  max_train = 7,
                                  n_predict = 1,
                                  model_arity = "uni")

df_after_loop_model_ex <- windowed_data_ex %>%
  rowmute(data_for_model = prepare_prediction_data(curr_data, id_time, 1, "y", "NA")) %>%
  rowmute(fit = simple_lm_func(data_for_model))

library(generics)
library(broom)
df_after_augment_ex <- df_after_loop_model_ex %>%
  pmute(aug_data = generics::augment(fit, newdata = curr_data))
collapse_if_exploded(df_after_augment_ex)

df_after_augment_ex
df_after_augment_ex %>%
  mutate(surv_data = calculate_surveillance_residuals(aug_data, id_time, 1)) %>%
  collapse_if_exploded()
# Ok, that's actually a lot cleaner than I expected! Awesome.

# So, what information do we need to run scan statistics?
# We need, at some level, to know the counts, the baseline, the zone, the n_mcsim,
# But of that, the only thing that should change with time are the counts and the baseline.
# In general, we're trying to split this into smaller functions. So calculating zones should happen elsewhere.
# The key question is...can I assume that id_space is 1:n?
# If so, then all the data I actually need is the data, the baseline_name and outcome_name

# And it should be something along the lines of
# scan_eb_poisson2(data, outcome_name, baseline_name, zones, n_mcsim = 0, gumbel = FALSE, max_only = FALSE)
# And then as long as data has the columns "id_time" and "id_space" then we're good to go.

space_coord <- sf::st_read("tests/testthat/three_zips/three_zips.shp",
                           quiet = TRUE) %>%
  dplyr::mutate(id_space = dplyr::row_number(), .before = dplyr::everything())

start_times <- 1:12
fin_times <- start_times + 1L
labels <- paste0("X", start_times) #sapply(start_times + 64, intToUtf8)
time_coord <- data.frame(time_labels = labels,
                         time = start_times,
                         fin_time = fin_times,
                         stringsAsFactors = FALSE)

data_for_scan <- tidyr::crossing(sf::st_drop_geometry(space_coord), time_coord) %>%
  dplyr::transmute(id_space,
                   id_time = as.numeric(stringr::str_sub(time_labels, 2)),
                   zcta_str,
                   time) %>%
  dplyr::arrange(id_space, id_time)

data_for_scan$baseline <- 4.3
data_for_scan$.sample1 <- 4.3
data_for_scan$.sample2 <- 6
# is_outbreak <- #space2 and space3, 10-13
is_outbreak <- data_for_scan$zcta_str %in% c("80401", "80203") & data_for_scan$time >= 9
data_for_scan$observed <- floor(data_for_scan$baseline + ifelse(is_outbreak, 4, 0))

windowed_for_scan <- window_idtime(data_for_scan, 7, Inf, 1, "multi")
# windowed_for_scan$augmented_data <- windowed_for_scan$curr_data

zones <- space_coord_to_zones(space_coord, max_k = 2) # Put in the minimum size of the zone here
alarm_res <- windowed_for_scan %>%
  rowmute(scan_eb_poisson = scan_eb_poisson2(curr_data, observed, baseline,
                                           zones, n_mcsim = 10),
          scan_permutation2(curr_data, observed))

alarm_res <- windowed_for_scan %>%
  rowmute(scan_eb_zip = scan_eb_zip2(curr_data, observed, baseline, zones, n_mcsim = 10))
  pmute(scan_eb_poisson_res = report_top_n_clusters(scan_eb_poisson, 3)) #Overlapping clusters.
plot_df <- alarm_res %>%
  # select(id_time, scan_eb_poisson_res) %>% # not neccessary, but makes things prettier.
  unnest(scan_eb_poisson_res)

library(ggplot2)
ggplot(plot_df, aes(x = id_time, y = score, color = factor(zone))) +
  geom_line() +
  geom_jitter(width = 0.2)

# Ok! That's like...super awesome! We've got a basic working prototype of what this all looks like
# There's a ton of tests and writing and features that are still to come, but I think
# This is already a big leap forward.

# Actually, the big test I always need to do is how easy/difficult is it to do this using pmute?

