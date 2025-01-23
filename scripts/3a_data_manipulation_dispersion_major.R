## 3. Data Manipulation
## a) Calculating dispersion for MAJOR benthic categories 
## Description: Calculating dispersion using major benthic categories only, assigning L, M, H categories
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 24th November 2023 (edited 6th Jan 2025)


## load libraries ------------------------------------------
install.packages("tidyverse")   # collection of packages (dplyr, ggplot2, readr tidyr etc)
install.packages("vegan")       # used to calculate dispersion
install.packages("ade4")        # used to calculate dispersion

library(tidyverse) 
library(data.table)
library(vegan)
library(ade4)


## load data ----
benthic_mean_major <- read.csv("data_output/benthic_transects_mean_major.csv", header = TRUE)  

# remove column 'X'
benthic_mean_major <- select(benthic_mean_major, -X)
  

## calculate dispersion ----
benthic_mean_major$sitename <- as.factor(benthic_mean_major$sitename)          # create factor for sitename = group for betadisper

benthic_maj_comm <- benthic_mean_major[,4:length(benthic_mean_major)]          # reformat as community matrix, select from column 4 to end (just major categories)

rownames(benthic_maj_comm) <- benthic_mean_major$site_transect                 # change rownames to site_transect names

benthic_maj_dist <- vegdist(benthic_maj_comm,                                  # create distance matrix - with euclidean
                                 method="euclidean", 
                                 binary=FALSE, 
                                 diag=FALSE, 
                                 upper=FALSE, 
                                 na.rm = FALSE) 

benthic_major_disp <- betadisper(benthic_maj_dist,                             # run dispersion - with distance to 'median'
                                  benthic_mean_major$sitename, 
                                  type = c("median"), 
                                  bias.adjust = FALSE, 
                                  sqrt.dist = FALSE, 
                                  add = FALSE)

disp_sites <- as.data.frame(benthic_major_disp$group.distances)                # extract site distance scores # outputs results file as dataframe


results_vector <- as.vector(benthic_major_disp$group.distances)

disp_sites <- rownames_to_column(disp_sites)                                   # need this step? # add rownames into dataframe (using dplyr)

disp_sites$rank <- NA                                                          # create empty column - rank 'results' from low to high 

colnames(disp_sites) <- c("sitename", "distances", "rank")                     # rename column names - rank 'results' from low to high 

disp_sites$rank[order(disp_sites$distances)] <- 1:nrow(disp_sites)             # rank 'results' from low to high

write.csv(file = "data_output/dispersion_major_site.csv", disp_sites)


## add dispersion category ----
# order sites low to high
disp_sites <- setorder(disp_sites, cols = distances)                           # calc quantiles and assign category (new column) +reorder factor levels to L, M, H

# create quartile column for dispersion results
disp_sites <- within(disp_sites,
                              quartile <- cut (x = distances,
                                               breaks = quantile(distances, probs = seq(0, 1, 0.25)),
                                               labels = FALSE, 
                                               include.lowest = TRUE))

# rename quartile column labels to 1= Low, 2 = Med, 3 = Med, 4 = High
disp_sites$dispersion <- NA                                                    # create a new empty column named dispersion

disp_sites$dispersion[disp_sites$quartile == 1] <- "Low"
disp_sites$dispersion[disp_sites$quartile > 1 & disp_sites$quartile <= 3] <- "Med"
disp_sites$dispersion[disp_sites$quartile == 4] <- "High"


write.csv(file = "data_output/dispersion_major_site.csv", disp_sites)





