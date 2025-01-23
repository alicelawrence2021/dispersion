## 3. Data Manipulation
## b) Calculating dispersion for MINOR coral genera categories 
## Description: Calculating dispersion using coral genera categories only, assigning L, M, H categories
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 24th November 2023 (edited 6th Jan 2025)


## load libraries ------------------------------------------
install.packages("tidyverse")   # collection of packages (dplyr, ggplot2, readr tidyr etc)
install.packages("data.table")  # used for photo selection randomisation process
install.packages("vegan")       # used to calculate dispersion
install.packages("ade4")        # used to calculate dispersion

library(tidyverse) 
library(data.table)
library(vegan)
library(ade4)


## load data ----
benthic_transects_mean_coral <- read.csv("data_output/benthic_transects_mean_coral.csv", header = TRUE)  

# remove column 'X'
benthic_transects_mean_coral <- select(benthic_transects_mean_coral, -X)


## calculate dispersion for coral ----
benthic_transects_mean_coral$sitename <- as.factor(benthic_transects_mean_coral$sitename)      # create factor for sitename = group for betadisper

benthic_coral_comm <- benthic_transects_mean_coral[,4:length(benthic_transects_mean_coral)]      # reformat as community matrix, select from column 4 to end (just major categories)

rownames(benthic_coral_comm) <- benthic_transects_mean_coral$site_transect                 # change rownames to site_transect names

benthic_coral_dist <- vegdist(benthic_coral_comm,                            # create distance matrix - with euclidean
                                 method="euclidean", 
                                 binary=FALSE, 
                                 diag=FALSE, 
                                 upper=FALSE, 
                                 na.rm = FALSE) 

benthic_coral_disp <- betadisper(benthic_coral_dist,                       # run dispersion - with distance to 'median'
                                 benthic_mean_coral$sitename, 
                                      type = c("median"), 
                                      bias.adjust = FALSE, 
                                      sqrt.dist = FALSE, 
                                      add = FALSE)

disp_coral_site <- as.data.frame(benthic_coral_disp$group.distances)            # extract site distance scores # outputs results file as dataframe


results_vector <- as.vector(benthic_coral_disp$group.distances)

disp_coral_site <- rownames_to_column(disp_coral_site)                          # need this step? # add rownames into dataframe (using dplyr)

disp_coral_site$rank <- NA                                              # create empty column - rank 'results' from low to high 

colnames(disp_coral_site) <- c("sitename", "distances", "rank")         # rename column names - rank 'results' from low to high 

disp_coral_site$rank[order(disp_coral_site$distances)] <- 1:nrow(disp_coral_site)       # rank 'results' from low to high

write.csv(file = "data_output/dispersion_coral_site.csv", disp_coral_site)


#### add dispersion category ####
# order sites low to high
disp_coral_site <- setorder(disp_coral_site, cols = distances)# calc quantiles and assign category (new column) +reorder factor levels to L, M, H

# create quartile column for dispersion results
disp_coral_site <- within(disp_coral_site,
                              quartile <- cut (x = distances,
                                               breaks = quantile(distances, probs = seq(0, 1, 0.25)),
                                               labels = FALSE, 
                                               include.lowest = TRUE))

# rename quartile column labels to 1= Low, 2 = Med, 3 = Med, 4 = High
disp_coral_site$dispersion <- NA                    # create a new empty column named dispersion

disp_coral_site$dispersion[disp_coral_site$quartile == 1] <- "Low"
disp_coral_site$dispersion[disp_coral_site$quartile > 1 & disp_coral_site$quartile <= 3] <- "Med"
disp_coral_site$dispersion[disp_coral_site$quartile == 4] <- "High"


write.csv(file = "data_output/dispersion_coral_site.csv", disp_coral_site)





