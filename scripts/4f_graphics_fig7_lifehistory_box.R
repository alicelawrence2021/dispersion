## 4. Graphics
## f) Figure 7 Visualisation of dispersion results - Life History
## Description: Summary boxplots for dispersion categories - Life History categories
## Alice Lawrence | alice.lawrence@bangor.ac.uk
##  1st July 2024 (edited 6th Jan 2025)


## load libraries ------------------------------------------
install.packages("tidyverse")   # collection of packages (dplyr, ggplot2, readr tidyr etc)
install.packages("gridExtra")   # Grid graphic objects
install.packages("forcats")     # 
install.packages("terra")       # for viridis colour palette?
install.packages("ggpubr")

library(tidyverse) 
library(gridExtra)  
library(forcats)
library(terra)    
library(ggpubr)


#import data
coral_trans <- read.csv("data_output/benthic_transects_mean_coral.csv")

disp_cat <- read.csv("data_output/dispersion_major_site.csv")


# split site_transect column into 2 new ones
coral_trans <- coral_trans %>% 
  tidyr::separate(site_transect, c("sitename", "transect"), sep = "_", remove = FALSE) 


# merge dataframe with MAJOR & Disp categories
# benthic_characteristics_model <- merge(disp_major_site_model, benthic_characteristics_model, by = 'sitename', all.x=TRUE)
coral_trans_disp <- merge(disp_cat, coral_trans, by = 'sitename', all.x=TRUE)


# remove unwanted columns
coral_trans_disp <- select(coral_trans_disp, -c(2, 3, 4,5, 7)) 


# convert to factors: sitename, rank, dispersion
coral_site_trans <- 
  coral_trans_disp %>% 
  mutate(across (c(sitename, site_transect, dispersion) , as.factor))

str(coral_site_trans)
levels(coral_site_trans$sitename)
levels(coral_site_trans$dispersion)


# reorder levels in dispersion category factor #
coral_site_trans$dispersion <- factor(coral_site_trans$dispersion,
                                      levels = c('Low', 'Med', 'High'),
                                      labels = c('Low', 'Med', 'High'))

levels(coral_site_trans$dispersion)
unique(coral_site_trans$dispersion)

# summary plots - reorder x axis to L - M - H
level_orderX <- c("Low", "Med", "High")


# save to file
write.csv(file = "data_output/coral_site_trans_forlifehist.csv", coral_site_trans)



#### mean and SE values per dispersion category ####

# MEAN coral genera per dispersion category

# coral_trans_disp_values <- select(coral_trans_disp, c(6, 8, 10, 11, 13:20)) # dont need this??

# coral_trans_disp_values <- group_by(coral_trans_disp_values, dispersion)

coral_trans_disp_values <- group_by(coral_trans_disp, dispersion)

coral_trans_disp_mean <- summarise_all(coral_trans_disp_values, mean)

coral_trans_disp_mean <- select(coral_trans_disp_mean, -c(2,3,4))

write.csv(file = "data_output/coral_trans_disp_mean_forlh.csv", coral_trans_disp_mean)


# same for se
# create function to calculate Standard Error SE...
std.error<-function(x){
  sqrt(var(x)/length(x))
}

coral_trans_disp_se <- summarise_all(coral_trans_disp_values, std.error)

coral_trans_disp_se <- select(coral_trans_disp_se, -c(2,3,4))

write.csv(file = "data_output/coral_trans_disp_se_forlh.csv", coral_trans_disp_se)


#### LIFE HISTORY GROUPS ####

# combine into one column
coral_site_trans_grps <- coral_site_trans %>% 
  mutate(ACROPALL = ACROP + ACROPARB + ACROST + ACROTBL) %>% 
  mutate(FAVIIDS = FAV + FAVT) %>% 
  mutate(RAPID = ACROPALL + POC + STYLO + PORCYL + MONTI) %>% 
  mutate(SLOW = PORMAS + FAV + FAVT + POR + PORRUS + ASTRP) %>% 
  mutate(COMPETITIVE = ACROPALL + POC) %>% 
  mutate(STRESS = FAVIIDS + PAV + LOBOPH) %>% 
  mutate(WEEDY = PORCYL + PORRUS + STYLO + LEPT + ISOP) %>% 
  mutate(GENERALIST = MONTI + MERU)


# RAPID
(rapid_disp <- ggplot(coral_site_trans_grps, aes(x = factor(dispersion, level = level_orderX),
                                       y = RAPID,
                                       fill = dispersion,
                                       width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(i) Rapid-growing corals") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)



# SLOW
(slow_disp <- ggplot(coral_site_trans_grps, aes(x = factor(dispersion, level = level_orderX),
                                                 y = SLOW,
                                                 fill = dispersion,
                                                 width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(ii) Slow-growing corals") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)



# COMPETITIVE
(comp_disp <- ggplot(coral_site_trans_grps, aes(x = factor(dispersion, level = level_orderX),
                                                y = COMPETITIVE,
                                                fill = dispersion,
                                                width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(v) Competitive corals") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)



# WEEDY
(weedy_disp <- ggplot(coral_site_trans_grps, aes(x = factor(dispersion, level = level_orderX),
                                                y = WEEDY,
                                                fill = dispersion,
                                                width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(iv) Opportunistic weedy corals") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)



# GENERALIST
(general_disp <- ggplot(coral_site_trans_grps, aes(x = factor(dispersion, level = level_orderX),
                                                y = GENERALIST,
                                                fill = dispersion,
                                                width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(iii) Generalist corals") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)


# STRESS
(stress_disp <- ggplot(coral_site_trans_grps, aes(x = factor(dispersion, level = level_orderX),
                                                y = STRESS,
                                                fill = dispersion,
                                                width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(vi) Stress-tolerant corals") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)


# 6 portrait with categories only
ggarrange(rapid_disp, slow_disp, general_disp, weedy_disp, comp_disp, stress_disp, 
          ncol=2, nrow=3, 
          common.legend = TRUE, 
          legend="none")

ggsave("graphs/Figure7_lifehist_box.pdf", width = 8, height = 12, dpi = 200)
ggsave("graphs/Figure7_lifehist_box.tiff", width = 8, height = 12, dpi = 200)








