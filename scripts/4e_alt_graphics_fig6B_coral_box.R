## 4. Graphics
## e) Visualisation of dispersion results - CORAL GENERA
## Description: Boxplots for dispersion cateogires for kwy coral genera
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 2nd July 2024 (edited 6th Jan 2025)


## load libraries ------------------------------------------
install.packages("tidyverse")   
install.packages("gridExtra")   
install.packages("forcats")      
install.packages("terra")       
install.packages("ggpubr")
install.packages("ggtext")      # add italicised text into plots


library(tidyverse) 
library(gridExtra)  
library(forcats)
library(terra)    
library(ggpubr)
library(ggtext)



#import data
# coral genera
coral_trans <- read.csv("data_output/benthic_transects_mean_coral.csv")

# remove  X column
coral_trans <- select(coral_trans, c(-X))

# split site_transect column into 2 new ones
coral_trans <- coral_trans %>% 
  tidyr::separate(site_transect, c("sitename", "transect"), sep = "_", remove = FALSE) 

# dispersion
disp_cat <- read.csv("data_output/dispersion_major_site.csv")

# remove  X column
disp_cat <- select(disp_cat, c(-X))



# merge dataframes
coral_trans_disp <- merge(disp_cat, coral_trans, by = 'sitename', all.x=TRUE)


# remove unwanted columns
coral_trans_disp <- select(coral_trans_disp, -c(2, 3, 4,7)) 


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
write.csv(file = "data_output/coral_site_trans.csv", coral_site_trans)



## Figure 6B SUMMARY BOXPLOTS Coral ALTERNATIVE Dec 2024 ----
# MEAN coral genera per dispersion category
coral_trans_disp_values <- group_by(coral_trans_disp, dispersion)

coral_trans_disp_mean <- summarise_all(coral_trans_disp_values, mean)

coral_trans_disp_mean <- select(coral_trans_disp_mean, -c(2,3,4))

write.csv(file = "data_output/coral_trans_disp_mean.csv", coral_trans_disp_mean)


# same for se
# create function to calculate Standard Error SE
std.error<-function(x){
  sqrt(var(x)/length(x))
}

coral_trans_disp_se <- summarise_all(coral_trans_disp_values, std.error)

coral_trans_disp_se <- select(coral_trans_disp_se, -c(2,3,4))

write.csv(file = "data_output/coral_trans_disp_se.csv", coral_trans_disp_se)


# create new column for all ACROPORA - 'ACROTBL', 'ACROP', 'ACROPARB', 'ACROTBL'
coral_wide <- coral_trans_disp %>% 
  mutate(ACROPORA = c(ACROTBL + ACROP + ACROPARB + ACROTBL))

# select columns 
coral_wide <- select(coral_wide, c(sitename, site_transect, dispersion, MONTI, ACROPORA, PAV, POC, ISOP, PORRUS)) 

# rename columns
coral_wide <- coral_wide %>%
  dplyr::rename(Montipora = MONTI, Acropora = ACROPORA, Isopora = ISOP, Porites_rus = PORRUS, Pocillopora = POC, Pavona = PAV)


# convert from wide format to long format - use pivot_longer
#library(tidyr)
coral_long <- coral_wide %>% 
  pivot_longer(
    cols = c('Montipora', 'Acropora', 'Pavona', 'Pocillopora', 'Isopora', 'Porites_rus'),
    names_to = "coral_genera",
    values_to = "cover"
  )


# reorder x axis to MONTI, ACROPORA, PAV, PORRUS, POC, ISOP
level_order_coral <- c("Montipora", "Acropora", "Pavona", "Porites_rus", "Pocillopora", "Isopora")


# reorder levels in coral genera
coral_long$coral_genera <- factor(coral_long$coral_genera,
                                      levels = c('Montipora', 'Acropora', 'Pavona', 'Porites_rus', 'Pocillopora', 'Isopora'),
                                      labels = c('Montipora', 'Acropora', 'Pavona', 'Porites rus', 'Pocillopora', 'Isopora'))

levels(coral_long$coral_genera)
unique(coral_long$coral_genera)


## LOW ----
# need to select LOW dispersion sites only
low_coral <- coral_long %>%
  filter(dispersion == "Low")

str(low_coral)


# Benthic CORAL summary plot
(low_coral_plot <- ggplot(low_coral, aes(x = coral_genera,
                                         y = cover,
                                         # fill = dispersion,
                                         width = 0.4)) +                                    
   geom_boxplot(width = 0.3, fill = "#fcc5c0") +
   geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
   ylim(0, 50) +
   labs(x = NULL, 
        y = "Mean % cover",
        #title = "Calcifying substrates",
        subtitle = "(i) Low Dispersion Sites") +
   theme_bw() +
   theme(plot.title = element_text(size = 14, hjust = 0.5), 
         plot.subtitle = element_text(size = 14, hjust = 0),
         text=element_text(family="sans", size=16), 
         axis.text.x = element_text(
           angle = 60, 
           face = "italic",
           vjust = 1, 
           hjust = 1),
         axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1),
         axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
         legend.position = "none")
 
)


## MED ----
# need to select MED dispersion sites only
med_coral <- coral_long %>%
  filter(dispersion == "Med")

str(med_coral)

# Benthic Major summary plot
(med_coral_plot <- ggplot(med_coral, aes(x = coral_genera,
                                         y = cover,
                                         # fill = dispersion,
                                         width = 0.4)) +                                    
    geom_boxplot(width = 0.3, fill = "#ec7014") +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    ylim(0, 50) +
    labs(x = NULL, 
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(ii) Medium Dispersion Sites") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5), 
          plot.subtitle = element_text(size = 14, hjust = 0),
          text=element_text(family="sans", size=16), 
          axis.text.x = element_text(angle = 60, face = "italic", vjust = 1, hjust = 1),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          legend.position = "none")
  
)



## HIGH ----
# need to select HIGH dispersion sites only
high_coral <- coral_long %>%
  filter(dispersion == "High")

str(high_coral)


# Benthic Major summary plot
(high_coral_plot <- ggplot(high_coral, aes(x = coral_genera,
                                           y = cover,
                                           # fill = dispersion,
                                           width = 0.4)) +                                    
    geom_boxplot(width = 0.3, fill = "#ae017e") +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    ylim(0, 50) +
    labs(x = "", 
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(iii) High Dispersion Sites") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5), 
          plot.subtitle = element_text(size = 14, hjust = 0),
          text=element_text(family="sans", size=16), 
          axis.text.x = element_text(angle = 60,  face = "italic", vjust = 1, hjust = 1),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          legend.position = "none")
  
)


#### ggarrange - summary - MAJOR - ALTERNATIVE ####

ggarrange(low_coral_plot, med_coral_plot, high_coral_plot, 
          ncol=1, nrow=3, 
          common.legend = TRUE, 
          legend="none")

ggsave("graphs/Figure6_alternative.pdf", width = 5, height = 14, dpi = 200)
ggsave("graphs/Figure6_alternative.tiff", width = 5, height = 14, dpi = 200)

ggsave("graphs/Fig 6_alt.pdf", width = 3, height = 10, dpi = 200)
ggsave("graphs/Fig 6_alt.tiff", width = 3, height = 10, dpi = 200)





