## 4. Graphics
## d) Coral genera nMDS
## Description: Loading ggvegan and creating nMDS
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 24th November 2023 (edited 6th Jan 2025)


## load libraries ----
# ggvegan not available on CRAN

install.packages("remotes")
install.packages("ggrepel")

remotes::install_github("gavinsimpson/ggvegan")

library(tidyverse)
library(vegan)
library(ggvegan)
library(ggrepel)

#### creating nMDS with ggvegan ####

# use 'metaMDS' function

# weblink: https://rstudio-pubs-static.s3.amazonaws.com/618343_95ac7f2f95af4866bbc876c91a75d8dc.html

#  imagine NMDS as a reduction of axes, where all your “axes” are the species within a sample, and each sample exists relative to others on the axes.


## load data ----
# coral genera data
coral_site_trans <- read.csv("data_output/benthic_transects_mean_coral.csv", header = TRUE) 

# create sitename column by splitting site_transect 
coral_site_trans <- coral_site_trans %>% 
  tidyr::separate(site_transect, c("sitename", "transect"), sep = "_", remove = FALSE) 

# remove  X and transect column
coral_site_trans <- select(coral_site_trans, c(-X, -transect))

# rename columns - coral genera names
coral_site_trans <- coral_site_trans %>%
  dplyr::rename(Pavona = PAV, Montipora = MONTI, Montastrea = MONT, Acropora = ACROP, Isopora = ISOP, Merulina = MERU, Galaxea = GAL, Porties_rus = PORRUS, Porites_cylindrica = PORCYL, Porites_massive = PORMAS)


# # re-label coral genera
# coral_site_trans$coral_genera <- factor(coral_site_trans$coral_genera,
#                                              levels = c('Montipora', 'Acropora', 'Pavona', 'Montastrea', 'Isopora', 'Merulina', 'Galaxea', 'Porites_rus', 'Isopora', 'Montipora', 'Porites_cylindrica', 'Porites_massive'),
#                                              labels = c('Montipora', 'Acropora', 'Pavona', 'Montastrea', 'Isopora', 'Merulina', 'Galaxea', 'Porites rus', 'Isopora', 'Montipora', 'Porites cylindrica', 'Porites massive'))


# dispersion data
dispersion_site_trans <- read.csv("data_output/dispersion_major_site.csv", header = TRUE)  
# remove X, distances, rank, quartile columns
dispersion_site_trans <- select(dispersion_site_trans, c(-X, -distances, -rank, -quartile))



## combine datasets  ----
# add dispersion_cat column to coral_site_trans
coral_site_trans_disp <- left_join(coral_site_trans, dispersion_site_trans, by = "sitename")

# change column name ... dispersion to Dispersion
coral_site_trans_disp <- coral_site_trans_disp %>%
  dplyr::rename(Dispersion = dispersion)

str(coral_site_trans_disp)


# reorder levels in dispersion category factor 
coral_site_trans_disp$Dispersion <- factor(coral_site_trans_disp$Dispersion,
                                levels = c('High', 'Med', 'Low'),
                                labels = c('High', 'Med', 'Low'))

levels(coral_site_trans_disp$Dispersion)
unique(coral_site_trans_disp$Dispersion)




## prepare dataset for NMDS ----

# change rownames to site_transect names
rownames(coral_site_trans) <- coral_site_trans$site_transect 

# remove unwanted columns
coral_site_trans <- select(coral_site_trans, -c(1:2))                          # remove blank columns 


#### create nMDS ####
major_nmds <- metaMDS(coral_site_trans)

major_nmds    # stress 0.27867 - best solution not repeated after 20 tries

#### stressplot ####

# look at stressplot to evaluate how well the ordination represented the complexity in your data
# the x-axis is the observed dissimilarity, and the y-axis is the ordination distance
# The stressplot shows you how closely the ordination (y-axis) represents the dissimilarities calculated (x-axis)
# The points around the red stair steps are the communities, and the distance from the line represents the “stress”, 
# or how they are pulled from their original position to be represented in their ordination.

stressplot(major_nmds)


# plot NMDS output in base R
plot(major_nmds)


# extract elements from the output to plot in ggplot
plot_df <- scores(major_nmds, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site_transect") %>% 
  full_join(coral_site_trans_disp, by = "site_transect")

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Dispersion)) +
  geom_point(size = 3, alpha = 0.8) +
  # scale_color_manual(values = dispersion) +
  #   clean_background +
  stat_ellipse(linetype = 2, size = 1) +
  labs(title = "NMDS")

plot_nmds


# adding overlays
# envfit() takes the output of metaMDS() and the species matrix you created
fit <- envfit(major_nmds, coral_site_trans, perm = 999) 

# extract p-values for each species
fit_pvals <- fit$vectors$pvals %>% 
  as.data.frame() %>% 
  rownames_to_column("species") %>% 
  dplyr::rename("pvals" = ".")


# extract coordinates for species, only keep species with p-val = 0.001
fit_spp <- fit %>% 
  scores(., display = "vectors") %>% 
  as.data.frame() %>% 
  rownames_to_column("species") %>% 
  full_join(., fit_pvals, by = "species") %>% 
  filter(pvals == 0.001)

# new plot
(nmds_plot_new <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  coord_fixed() +
  geom_point(aes(size = "CORAL", color = Dispersion), size = 3, alpha = 0.8) +   # size not working
  stat_ellipse(aes(color = Dispersion)) +
  #scale_color_manual(values = dispersion) +
  scale_color_manual(values = c("#D55E00", "#56B4E9", "#999999")) +
  geom_segment(data = fit_spp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               col = "black") +
  geom_text(data = fit_spp, aes(label = species)) +
  theme_bw()+
  theme(axis.text.x = element_text(color = "grey20", size =16), 
        axis.title.x = element_text(color = "grey20", size = 16, margin=ggplot2::margin(t=12)),
        axis.text.y = element_text(color = "grey20", size = 16), 
        axis.title.y = element_text(color = "grey20", size = 16, margin=ggplot2::margin(r=12)),
        plot.title = element_text(color = "grey20", size = 16, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text=element_text(size=12), legend.title=element_text(size=12),
        legend.key = element_rect(fill = "white"),
        legend.position = "inside", 
        legend.position.inside = c(0.092, 0.88)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

)

??ggrepel   # tried to use this to 

ggsave("graphs/Figure6A_nmds.tiff", width = 12, height = 6, dpi = 200)
ggsave("graphs/Figure6A_nmds.pdf", width = 12, height = 6, dpi = 200)


  

  
  
  
  
  
  
