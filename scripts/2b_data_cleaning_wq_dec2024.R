## 2. Data Cleaning 
## b) water quality data
## Description: calculating SD and SE for DIN values per site
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## Dec 2024 (reanalysis following journal review)

## Libraries ----
install.packages("tidyverse")
install.packages("ggplot2")

library(tidyverse) 
library(ggplot2)  

## Load data ----
wq <- read.csv("data/wq_dec2024.csv", header = TRUE)  


# Check import and preview data - using tidyverse (more efficient)
tibble::as_tibble(wq)
glimpse(wq)


# change column names: village to "sitename", site to "stream"
wq <- wq %>%
  dplyr::rename(sitename = village, stream = site)


## calculate mean, SD and SE ----

# create function to calculate Standard Error (SE)
std.error<-function(x){
  sqrt(var(x)/length(x))
}

# calculate mean/sd/se by stream 
stream_wq <- 
  wq %>%
  group_by(stream, sitename) %>%
  summarise(mean_stream = mean(din), sd_stream = sd(din), se_stream = std.error(din), max_stream = max(din))

# calculate mean/sd/se by site
site_wq <- 
  wq %>%
  group_by(sitename) %>%
  summarise(mean_din = mean(din), sd_din = sd(din), se_din = std.error(din), max_din = max(din))


# check if calculation is correct
# e.g. Alofau streams 0.26318182 + 0.13492308
x = (0.26318182 + 0.13492308)/2


# remove sitename "Aua"
site_wq <- site_wq %>%
  filter(sitename !="Aua")


# save data to csv
write.csv(file = "data_output/site_wq.csv", site_wq)

