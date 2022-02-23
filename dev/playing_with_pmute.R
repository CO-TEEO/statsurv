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

windowed_data <- window_spacetime(spacetime_data,
                                  min_train = 5,
                                  max_train = 7,
                                  n_predict = 1,
                                  model_arity = "multi") %>%
  .[6:12, ]

windowed_data %>%
  mutate(fit = list(lm(y ~ time, data = curr_data)))

df <- windowed_data %>%
  pmute(fit = lm(y ~ id_time, data = curr_data),
        q = list(id_time),
        q2 = id_time)

map_dfc(df, check_and_delist)
check_and_delist(df$fit)
check_and_delist(df$q2)

cn <- colnames(df)
for (nm in cn) {
  df[[nm]] <- check_and_delist(df[[nm]])
}

gdf <- df %>%
  group_by(g = q %% 2) %>%
  select(id_time, q, g)
str(gdf)

g2 <- gdf %>%
  groups()

rdf <- gdf %>%
  rowwise()
rdf %>%
  group_by(!!!g2)

# Final step: Can we make progress bars work?
split_df <- split(rowwise(windowed_data), seq_len(nrow(windowed_data)))
map(split_df, function(x) mutate(x, y = id_time + 2)) %>%
  bind_rows()

library(progressr)
handlers("progress")
with_progress({
  df <- windowed_data %>%
    pmute(fit = lm(y ~ id_time, data = curr_data),
          q = list(id_time),
          q2 = id_time)
})

# Ok, next question:
# How much overhead is this adding to my system?
# My guess it that it's going to come from the progress updates, because I have to split and re-join.
big_df <- replicate(windowed_data, n = 100, simplify = FALSE) %>%
  bind_rows()

f1 <- function(df) {
  df %>%
    pmute(fit = lm(y ~ id_time, data = curr_data),
          q = list(id_time),
          q2 = id_time)
}
f1(df)

f2 <- function(df) {
  df %>%
    rowwise() %>%
    mutate(fit = list(lm(y ~ id_time, data = curr_data)),
           q = id_time,
           q2 = id_time) %>%
    ungroup()
}
f2(df)

bench::mark(f1(big_df), f2(big_df))
# Yes, that's pretty different.
# That's a factor of 15 decrease.



f1(big_df)
giant_df <- replicate(windowed_data, n = 1000, simplify = FALSE) %>%
  bind_rows()
f1(giant_df)
options(statsurv.progress = FALSE)
f1(giant_df)
f2(giant_df)

bench::mark(f1(big_df), f2(big_df))
f1(big_df)
f2(big_df)

options(statsurv.progress = TRUE)
f1(big_df)

#### What does this look like now?

df_after_loop_model <- windowed_data %>%
  pmute(data_for_model = prep_data_for_model(curr_data, id_time, 1, "y", "NA")) %>%
  pmute(fit = simple_lm_func(data_for_model))

library(generics)
library(broom)
df_after_augment <- df_after_loop_model %>%
  pmute(aug_data = generics::augment(fit, newdata = curr_data))
# Not sure how much we'll be able to use augment. It seems like it's pretty finicky and might not work
# with forecast or fable.

# Ok, now I need to convert to surveillance. Maybe?
# I'm not actually sure that I need to.
# I think generally we'd say not?
# But it would be good to have the option.
# And that's one where we'd want to clarify that it's not rowwise (because we have to combine them)


windowed_data_ex <- window_spacetime(spacetime_data,
                                  min_train = 5,
                                  max_train = 7,
                                  n_predict = 1,
                                  model_arity = "uni") %>%
  filter(id_time >= 6)

df_after_loop_model_ex <- windowed_data_ex %>%
  pmute(data_for_model = prep_data_for_model(curr_data, id_time, 1, "y", "NA")) %>%
  pmute(fit = simple_lm_func(data_for_model))

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

windowed_for_scan <- window_spacetime(data_for_scan, 7, Inf, 1, "multi")
# windowed_for_scan$augmented_data <- windowed_for_scan$curr_data

zones <- space_coord_to_zones(space_coord, max_k = 2) # Put in the minimum size of the zone here
alarm_res <- windowed_for_scan %>%
  pmute(scan_eb_poisson = scan_eb_poisson2(curr_data, "observed", "baseline",
                                           zones, n_mcsim = 10)) %>%
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

