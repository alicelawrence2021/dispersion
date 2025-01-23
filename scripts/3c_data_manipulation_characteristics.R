## 3. Data Manipulation
## c) Benthic characteristics 
## Description: Calculating benthic characteristics for DISTLM model, including diversity indices and BSR
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 15 Jan 2024 (edited 6th Jan 2025)


## load libraries ------------------------------------------
install.packages("tidyverse")   # collection of packages (dplyr, ggplot2, readr tidyr etc)
install.packages("vegan")       # used to calculate dispersion, diversity, MDS, PCA etc
install.packages("ggvegan")     
install.packages("ade4")        # used to calculate dispersion

library(tidyverse) 
library(vegan)
library(ggvegan)
library(ade4)

# # error installing ggvegan
# if (!require(devtools)) {
#   install.packages("devtools")
# }
# devtools::install_github("gavinsimpson/ggvegan")
# devtools::install_github("githubadress")


## load data ----

## load benthic site major ----
benthic_site_mean_major <- read.csv(file = "data_output/benthic_site_mean_major.csv", header = TRUE)
# remove column 'X'
benthic_site_mean_major <- select(benthic_site_mean_major, -X)


## load benthic site_transect major ----
benthic_transects_mean_major <- read.csv(file = "data_output/benthic_transects_mean_major.csv", header = TRUE)
# remove column 'X'
benthic_transects_mean_major <- select(benthic_transects_mean_major, -X)


## load coral site_transect  ----
benthic_transects_mean_coral <- read.csv(file = "data_output/benthic_transects_mean_coral.csv", header = TRUE)
# remove column 'X'
benthic_transects_mean_coral <- select(benthic_transects_mean_coral, -X)


## load coral site data ----
benthic_site_mean_coral <- read.csv(file = "data_output/benthic_site_mean_coral.csv", header = TRUE)
# remove column 'X'
benthic_site_mean_coral <- select(benthic_site_mean_coral, -X)


## load benthic major dispersion data ----
disp_major_site <- read.csv(file = "data_output/dispersion_major_site.csv", header = TRUE)
# remove column 'X'
disp_major_site <- select(disp_major_site, -X)


## load predictors (calculate from PRIMER) ----
benthic_predictors <- read.csv(file = "data/predictors_all_hali.csv", header = TRUE, fileEncoding="UTF-8-BOM")
# select only diversity measures (benthic cover is old version)
benthic_predictors <- select(benthic_predictors, c(1, 7, 8, 15:18))
        
                       
## calculate BSR - site level ----
benthic_site_mean_major_bsr <- benthic_site_mean_major %>% 
  mutate(calcifiers = CORAL + CCA + BCA + HALI) %>% 
  mutate(noncalcifiers = MA_noHALI + TURF + FCA) %>% 
  mutate(BSR = calcifiers / noncalcifiers)

write.csv(file = "data_output/benthic_site_mean_major_bsr.csv", benthic_site_mean_major_bsr)

## calculate BSR - site-transect level ----
benthic_site_transects_major_bsr <- benthic_transects_mean_major %>% 
  mutate(calcifiers = CORAL + CCA + BCA + HALI) %>% 
  mutate(noncalcifiers = MA_noHALI + TURF + FCA) %>% 
  mutate(BSR = calcifiers / noncalcifiers)

write.csv(file = "data_output/benthic_site_transects_major_bsr.csv", benthic_site_transects_major_bsr)


# #### for DISTLM (not needed?) create dataframe with selected benthic characteristics + dispersion ####
# 
# # select only columns needed
# benthic_characteristics_model <- select(benthic_site_mean_major_bsr, c(2:4, 6:8, 10:13, 16))
# 
# benthic_predictors_model <- select(benthic_predictors, c(1,3, 6, 7))
# 
# disp_major_site_model <- select(disp_major_site, c(2:3))
# 
# # add diversity metrics and dispersion columns (cbind = column bind)
# benthic_characteristics_model <- plyr::join(benthic_predictors_model, benthic_characteristics_model, by = 'sitename')
# 
# benthic_characteristics_model <- merge(disp_major_site_model, benthic_characteristics_model, by = 'sitename', all.x=TRUE)
# 
# write.csv(file = "data_output/benthic_characteristics_model.csv", benthic_characteristics_model)


