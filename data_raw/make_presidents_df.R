library(dplyr)
library(lubridate)

presidents_df <- data.frame(approval_rating = as.numeric(presidents),
                            date = seq(start(presidents)[[1]] + start(presidents)[[2]]/4,
                                       end(presidents)[[1]] + end(presidents)[[2]]/4,
                                       by = 0.25)) %>%
  dplyr::mutate(election_year = as.numeric(floor(date) %% 4 == 0))
usethis::use_data(presidents_df, overwrite = TRUE)
