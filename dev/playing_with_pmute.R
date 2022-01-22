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


