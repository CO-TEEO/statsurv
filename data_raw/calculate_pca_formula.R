#calculating our pca formula
# Goal here is to use a fix subset of the census data, calculate PCA, and then save the formula, so
# we don't have to count on the PCA calculating things the same way every time.
# This should be done *manually*, so someone can take a look at what the resulting components looks like.

### Libraries ###
library("lubridate")
library("tidycensus")
library("rgdal")
library("magrittr")
library("dplyr")
library("tidyr")
library("ggplot2")


### Parameters ###
TO_SAVE <- TRUE
SURVEILLANCE_AREA <- readOGR(file.path("data_raw",
                                       "geospatial_data",
                                       "denver_metro_boundary",
                                       "denver_metro_boundary.shp"))
START_DATE <- ymd("20120101")
END_DATE <- ymd("20181231")
tidycensus::census_api_key("648470ebccdc7b8dc0832bd7c734c894f5e69774")




### Helper functions ###
abs_sort <- function(pc) {
  sort_data <- sort(abs(pc), index.return = TRUE, decreasing = TRUE)
  pc[sort_data$ix]
}

tall_correlations <- function(correlation_matrix) {
  cor_df <- data.frame(correlation_matrix)
  cor_df$Var1 <- factor(rownames(cor_df), levels = wanted_columns[wanted_columns %in% rownames(cor_df)])
  tidyr::pivot_longer(cor_df,
                      cols = -Var1,
                      names_to = "Var2",
                      values_to = "value",
                      names_ptypes = list("Var2" = factor(levels = colnames(cor_df))))
}

### Calculations ###
# Get census data, select out some useful columns
date_range <- generate_date_range(START_DATE, END_DATE, time_range = "quarter")
study_area <- generate_study_area(SURVEILLANCE_AREA, space_range = "tract")
covariates <- generate_census_covariate_data(date_range, study_area)

wanted_columns <- c("pcnt_foreignborn", "pcnt_nhaa", "pcnt_nhasian",
                    "pcnt_nhwhite", "pcnt_nhamerindian", "pcnt_hispanic",
                    "pcnt_movedcounty", "pcnt_movedwicounty", "pcnt_movedstate",
                    "pcnt_renter", "pcnt_hu_vacant", "pcnt_households_belowfpl",
                    "pcnt_households_publicassistancesnap", "pcnt_households_novehicle",
                    "pcnt_households_singleparent",
                    "pcnt_popover16_unemployed", "households_medianincome", "pcnt_popover25_less12grade",
                    "pcnt_under6_less200fpl", "pcnt_under6_less100fpl", "pcnt_under6")

# Filter to only one date
data_for_pca <- covariates %>%
  dplyr::filter(date_label == "2018Q4") %>%
  .[, wanted_columns] %>%
  tidyr::drop_na()

# Center and scale all variables
data_for_pca_z <- data_for_pca %>%
  transmute_all(function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))



# Plot the correlation coefficients
gathered_cor <- tall_correlations(cor(data_for_pca_z, use = "na.or.complete"))
p1 <- ggplot(data = gathered_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_raster() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  geom_rect(aes(xmin = 11.5, xmax = 20.5, ymin = 11.5, ymax = 20.5), color = "black", fill = NA, lwd = 1) +
  annotate("text", x = 15.25, y = 19.75, label = "wealth", size  = 3.5) +

  geom_rect(aes(xmin = 0.5, xmax = 6.5, ymin = 11.5, ymax = 20.5), color = "black", fill = NA) +
  annotate("text", x = 3.5, y = 19.75, label = "race * wealth", size = 3.5) +

  geom_rect(aes(xmin = 6.5, xmax = 11.5, ymin = 6.5, ymax = 11.5), color = "black", fill = NA) +
  annotate("text", x = 8.25, y = 10.75, label = "Moving", size = 3.5) +

  geom_rect(aes(xmin = 0.5, xmax = 6.5, ymin = 0.5, ymax = 6.5), color = "black", fill = NA) +
  annotate("text", x = 2.5, y = 5.75, label = "Ethnicity", size = 3.5) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))
print(p1)
# Calculate the principle components
prin_comps <- prcomp(data_for_pca_z, retx = TRUE, center = FALSE, scale. = FALSE, tol = 0.01)

#Visualize the results
gathered_rotation <- tall_correlations(prin_comps$rotation)
gathered_rotation$pos <- ifelse(abs(gathered_rotation$value) > 0.25, 1, NA)
p2 <- ggplot(data = gathered_rotation, aes(y = Var1, x = Var2, fill = value)) +
  geom_raster() +
  geom_point(aes(x = ifelse(!is.na(pos), Var2, NA)), color = "gray30") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0, limit = c(-0.5,0.5), space = "Lab",
                       name="Pearson\nCorrelation",
                       oob = scales::squish) +
  scale_y_discrete(limits = rev(levels(gathered_rotation$Var1))) +
  geom_text(aes(y = 24, label = substr(Var2,3, 5))) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) +
  coord_cartesian(clip = "off")
print(p2)

plot(prin_comps$sdev, main = "Standard deviation of the Principal Components")


#Principal Component 1
print(abs_sort(prin_comps$rotation[, 1]))
# Large contributions from low snap, high nh-white, low fpl, low less hs, low fpl, low single
# parent, high median income, low hispanic, low foreign born, low renter. Incorporates a lot of
# diferent components, but they all seem to relate to socio-economic status - so we'll refer to it
# as "pca1_ses"

#Principal Component 2
print(abs_sort(prin_comps$rotation[, 2]))
# high moved state, high renter, high vacancy, high moved county and wi county, low children.
# Definitely seems related to something to do with stability and movement rates - refer to it as
# "pca_stability"

#Principal Component 3
print(abs_sort(prin_comps$rotation[, 3]))
# Largest comtributors are high asian, high foreign born, high nh-aa, low nh-white. Refer to it as
# "pca3_ethnicity"

pca_coefficients <- list("pca1_ses" = prin_comps$rotation[, 1],
                         "pca2_stability" = prin_comps$rotation[, 2],
                         "pca3_ethnicity" = prin_comps$rotation[, 3])


if (TO_SAVE) {
  user_selection = menu(c("Yes", "No"),
                        graphics = FALSE,
                        title = "Save these pca coefficients?")
  if (user_selection == 1) {
    dir <- file.path("data_raw",
                     "pca_coeff")
    if (!dir.exists(dir)) {
      dir.create(dir)
    }
    full_fn <- file.path(dir,
                         paste0("pca_coeff_",
                                strftime(today(), format = "%y%m%d"),
                                ".RDS"))
    saveRDS(pca_coefficients,
            file = full_fn)
  }
}
