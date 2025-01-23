## 2. Data Cleaning 
## a) benthic data
## Description: Data cleaning: removing sites, removing blank benthic minor categories, rename CHRYS to Rubble, and other labels
## Description: Creating data frames: mean/sd/se for (1) benthic major per site_transects, (2) coral per Site_transects (3) benthic major per site, (4) coral per site
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 1st May 2023 (edited 6th Jan 2025)


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

benthic <- read.csv("data/benthic_raw_v2.csv", header = TRUE)  

# changes to benthic from previous - changed back sector S to SE, added MA_noHALI, changed Tafeu to NE (as per NOAA sectors) - previous file

# Check import and preview data  
tibble::as_tibble(benthic)
glimpse(benthic)


## data cleaning ----

# renaming variables 
benthic <- benthic %>%
  dplyr::rename(CORAL = C, RUB = CHRYS, Rubble = CHRYOBRN, Tur = TURF, TURF = T, SAND = SD, Rock = G)


# remove columns containing minor categories with no data
benthic <- select(benthic, c(-TWS, -ACAN, -EUPH, -GARD, -HELIO, -MYCED, -OULO, -PECT, -PHYSO, -PLERO, -SCAP, -STYLC, -ASC, -CUPS, -DISCO, -DYS, -NoIDINV, -OLV, -TERPS, -Bood, -BRYP, -CLP, -LIAG, -LOBO, -MAST, -MICDTY, -PAD, -SARG, -SCHIZ, -TURB, -AMP, -JAN, -Shadow, -Tape, -Wand, -BC, -HERP, -PLSIA, -SANDO, -SERIA, -TYDM)) 


# remove Aua site (an outlier)
benthic <- benthic %>%
  filter(sitename !="Aua")


# remove duplicate sites - Fagasa 1 & Leone 1
benthic <- benthic %>%
  filter(!sitename %in% c("Fagasa1", "Leone1"))

unique(benthic$sitename)        # check list of site names
unique(benthic$sector)  


# combine and add column to label site_transect
benthic$site_transect <- paste(benthic$sitename, sep = "_", benthic$transect_no)

write.csv(file = "data_output/benthic_by_site_transects.csv", benthic)


## calculate site_transect means ----

# calculate mean for each of the 6 transects for each site
  benthic_transects <- group_by(benthic, site_transect)                        # group by site_transect

  benthic_transects_mean <- summarise_all(benthic_transects, mean)             # calculate mean per site_transect 
  
  benthic_transects_mean <- select(benthic_transects_mean, -c(2:15))           # remove unecessary columns 
  
  write.csv(file = "data_output/benthic_transects_mean.csv", benthic_transects_mean)
  
  
  # calculate SD for each of the 6 transects for each site
  benthic_transects <- group_by(benthic, site_transect)                        # group by site_transect
  
  benthic_transects_sd <- summarise_all(benthic_transects, sd)                 # calculate SE per site_transect 
  
  benthic_transects_sd <- select(benthic_transects_sd, -c(2:15))               # remove blank columns 
  
  write.csv(file = "data_output/benthic_transects_sd.csv", benthic_transects_sd)
  
  
  # calculate SE for each of the 6 transects for each site #
  
  # create function to calculate Standard Error (SE)
  std.error<-function(x){
  sqrt(var(x)/length(x))
  }
  
  benthic_transects <- group_by(benthic, site_transect)                        # group by site_transect
  
  benthic_transects_se <- summarise_all(benthic_transects, std.error)          # calculate SE per site_transect 
  
  benthic_transects_se <- select(benthic_transects_se, -c(2:15))               # remove blank columns 
  
  write.csv(file = "data_output/benthic_transects_se.csv", benthic_transects_se)
  
  

# mean benthic MAJOR only (including HALI and MA_noHALI - remove MA)
  
  benthic_transects_mean_major <- select(benthic_transects_mean, 1:3, 5:11, 64) # select MAJOR benthic categories only
  
  benthic_transects_mean_major <- separate(benthic_transects_mean_major,       # separate site_spc column into 2 new columns
                                           site_transect, 
                                into = c("sitename", "transect_no"), 
                                sep = "_", 
                                remove = FALSE)
  
  write.csv(file = "data_output/benthic_transects_mean_major.csv", benthic_transects_mean_major)
  
 
  # mean coral MINOR only
  
  benthic_transects_mean_coral <- select(benthic_transects_mean, 1, 12:52)     # select CORAL genera only
  
  write.csv(file = "data_output/benthic_transects_mean_coral.csv", benthic_transects_mean_coral)
  
  
  
  ## calculate site means ----
  
  # calculate site mean of the 6 transects 
  benthic_transects_mean <- separate(benthic_transects_mean,                   # separate site_spc column into 2 new columns
                                     site_transect, 
                                     into = c("sitename", "transect_no"), 
                                     sep = "_", 
                                     remove = FALSE)
  benthic_transects_mean <- group_by(benthic_transects_mean, sitename)                  
  benthic_site_mean <- summarise_all(benthic_transects_mean, mean)            
  benthic_site_mean <- select(benthic_site_mean, -c(2:3))                
  
  write.csv(file = "data_output/benthic_transects_mean.csv", benthic_site_mean)
  
      
      
  # calculate site sd of the 6 transects #
  benthic_transects_mean <- group_by(benthic_transects_mean, sitename)                  
  benthic_site_sd <- summarise_all(benthic_transects_mean, sd)             
  benthic_site_sd <- select(benthic_site_sd, -c(2:3))              
      
  write.csv(file = "data_output/benthic_transects_sd.csv", benthic_site_sd)
      
      
  # calculate site se of the 6 transects #
  benthic_transects_mean <- group_by(benthic_transects_mean, sitename)                  
  benthic_site_se <- summarise_all(benthic_transects_mean, std.error)             
  benthic_site_se <- select(benthic_site_se, -c(2:3))              
      
  write.csv(file = "data_output/benthic_transects_se.csv", benthic_site_se)
      
  
  # site mean benthic MAJOR only (including HALI and MA_noHALI. Remove MA)
  
  benthic_site_mean_major <- select(benthic_site_mean, 1:3, 5:11, 64)          # select MAJOR benthic categories only
  
  write.csv(file = "data_output/benthic_site_mean_major.csv", benthic_site_mean_major)
  
  
  # site mean coral MINOR only
  
  benthic_site_mean_coral <- select(benthic_site_mean, 1, 12:52)               # select CORAL genera only
  
  write.csv(file = "data_output/benthic_site_mean_coral.csv", benthic_site_mean_coral)
  
  

  ## convert multiple columns to factors - use as.factor() ----
  benthic <- 
    benthic %>% 
    mutate(across (c(sitename, sector, geography, watershed, reeftype, transect_no, photo_no, original_photo_no, spc_no, site_transect) , as.factor))
  
  str(benthic)
  