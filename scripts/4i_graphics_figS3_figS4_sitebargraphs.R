## 4. Graphics
## i) Visualisation of dispersion results by site (fig S3 & S4)
## Description: Calculate BSR, bar graphs sites ranked with benthic major and coral genera, MDS plots
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 1st May 2023 (edited 6th Jan 2025)


## load libraries ----
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


## import data ----
# benthic major groups
benthic_site_mean <- read.csv("data_output/benthic_site_mean_major.csv")
# remove X column
benthic_site_mean <- select(benthic_site_mean, -X)

# coral genera 
coral_site_mean <- read.csv("data_output/benthic_site_mean_coral.csv")
# remove X column
coral_site_mean <- select(coral_site_mean, -X)

# standard error for all groups
site_se <- read.csv("data_output/benthic_transects_se.csv")
# remove X column
site_se <- select(site_se, -X)

# benthic dispersion per site
disp_site <- read.csv("data_output/dispersion_major_site.csv")
# remove X , rank, quartile columns
disp_site <- select(disp_site, c(-X, -quartile))


## combine datasets  ----
# add dispersion data to benthic_site_mean
major_trans <- left_join(benthic_site_mean, disp_site, by = "sitename")

# add dispersion data to coral_site_mean
coral_trans <- left_join(coral_site_mean, disp_site, by = "sitename")



## prepare data for graphics ----

# Figure 1 - Benthic Major % cover ranked and coloured by disp category

# Figure 2 - Coral genera % cover ranked and coloured by disp category


# convert to factors: sitename, rank, dispersion
major_trans <- 
  major_trans %>% 
  mutate(across (c(sitename, rank, dispersion) , as.factor))

coral_trans <- 
  coral_trans %>% 
  mutate(across (c(sitename, rank, dispersion) , as.factor))

str(major_trans)
str(coral_trans)
levels(major_trans$sitename)


# reorder levels in dispersion category factor
#major_trans
major_trans$dispersion <- factor(major_trans$dispersion,
                           levels = c('Low', 'Med', 'High'),
                           labels = c('Low', 'Med', 'High'))

levels(major_trans$dispersion)

unique(major_trans$dispersion)

# coral_trans
coral_trans$dispersion <- factor(coral_trans$dispersion,
                                 levels = c('Low', 'Med', 'High'),
                                 labels = c('Low', 'Med', 'High'))

levels(coral_trans$dispersion)

unique(coral_trans$dispersion)

#reorder factor levels - benthic
major_trans$sitename <- factor(major_trans$sitename,
                         levels = c('Oa', 'Fagaitua', 'Leone2', 'Masausi', 'Amalau', 'Poloa', 'Maloata', 'Sita', 'Nuuuli', 'Alega', 'Fagatele', 'Fagaalu', 'Afono', 'Amanave', 'Fagamalo', 'Amouli', 'Matuu', 'Amaluia', 'Nua', 'Tafeu', 'Aoa', 'Masefau',  'Amaua', 'Alofau', 'Laulii', 'Fagasa2', 'Vatia', 'Aasu'))

levels(major_trans$sitename)

unique(major_trans$sitename)


#reorder factor levels - coral
coral_trans$sitename <- factor(coral_trans$sitename,
                               levels = c('Oa', 'Fagaitua', 'Leone2', 'Masausi', 'Amalau', 'Poloa', 'Maloata', 'Sita', 'Nuuuli', 'Alega', 'Fagatele', 'Fagaalu', 'Afono', 'Amanave', 'Fagamalo', 'Amouli', 'Matuu', 'Amaluia', 'Nua', 'Tafeu', 'Aoa', 'Masefau',  'Amaua', 'Alofau', 'Laulii', 'Fagasa2', 'Vatia', 'Aasu'))

levels(coral_trans$sitename)

unique(coral_trans$sitename)

# graphic 1 - benthic major % cover ordered by dispersion rank on x
# graphic1 <- ggplot(major_trans, aes(x = sitename, y = Coral)) +
#   geom_col()
# 
# plot(graphic1)


#### benthic major graphs ####
# Major Benthic categories
c <- (ggplot(major_trans, aes(x = sitename, 
                                  y = CORAL, 
                                  fill = dispersion,
                                  width = 0.4)) +
  geom_col() +
    ylim(0, 60) +
  viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                              name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
  labs(x = "", 
       y = "% cover",
       title = "Calcifying substrate",
       subtitle = "(i) Hard Coral") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5), 
        #subtitle = element_text(size = 12, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")) 
plot(c)


# turf
ta <- (ggplot(major_trans, aes(x = sitename, 
                        y = TURF, 
                        fill = dispersion,
                        width = 0.4)) +
         geom_col() +
             ylim(0, 60) +
         viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                     name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
        labs(x = "", 
             y = "% cover",
             title = "",
             subtitle = "(v) Turf Algae") +
        theme_bw() +
        theme(plot.title = element_text(size = 12, hjust = 0), 
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.position = "none")) 

plot(ta)


# cca
cca <- (ggplot(major_trans, aes(x = sitename, 
                         y = CCA, 
                         fill = dispersion,
                         width = 0.4)) +
          geom_col() +
          ylim(0, 60) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "", 
               y = "% cover",
               title = "",
               subtitle = "(ii) Crustose Coralline Algae (CCA)") +
          theme_bw() +
         theme(plot.title = element_text(size = 12, hjust = 0), 
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               legend.position = "none")) 

plot(cca)


# macroalgae
ma <- (ggplot(major_trans, aes(x = sitename, 
                         y = MA_noHALI, 
                         fill = dispersion,
                         width = 0.4)) +
         geom_col() +
         ylim(0, 60) +
         viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                     name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
         labs(x = "Site", 
              y = "% cover",
              title = "",
              subtitle = "(vi) Macroalgae (non-calcified)") +
         theme_bw() +
         theme(plot.title = element_text(size = 12, hjust = 0), 
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               legend.position = "none")) 
plot(ma)


# BCA
bca <- (ggplot(major_trans, aes(x = sitename, 
                         y = BCA, 
                         fill = dispersion,
                         width = 0.4)) +
         geom_col() +
          ylim(0, 60) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                     name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "Site", 
               y = "% cover",
               title = "",
               subtitle = "(iii) Branching Coralline Algae (BCA)") +
         theme_bw() +
         theme(plot.title = element_text(size = 12, hjust = 0), 
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               legend.position = "none")) 
plot(bca)


# FCA
fca <- (ggplot(major_trans, aes(x = sitename, 
                                y = FCA, 
                                fill = dispersion,
                                width = 0.4)) +
          geom_col() +
          ylim(0, 60) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "", 
               y = "% cover",
               title = "Non-calcifying substrate",
               subtitle = "(iv) Fleshy Coralline Algae (FCA)") +
          theme_bw() +
          theme(plot.title = element_text(size = 12, hjust = 0.5), 
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none")) 
plot(fca)


# Sand
sand <- (ggplot(major_trans, aes(x = sitename, 
                                y = SAND, 
                                fill = dispersion,
                                width = 0.4)) +
          geom_col() +
           ylim(0, 60) +
           viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "", y = "% cover") +
          ggtitle('(i) Sand') +
          theme_bw() +
          theme(plot.title = element_text(size = 12, hjust = 0), 
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none")) 
plot(sand)


# Rubble
rub <- (ggplot(major_trans, aes(x = sitename, 
                                 y = RUB, 
                                 fill = dispersion,
                                 width = 0.4)) +
           geom_col() +
          ylim(0, 60) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                       name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
           labs(x = "", y = "% cover") +
           ggtitle('(ii) Rubble') +
           theme_bw() +
           theme(plot.title = element_text(size = 12, hjust = 0), 
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                 legend.position = "none")) 
plot(rub)


# Other Inverts
oi <- (ggplot(major_trans, aes(x = sitename, 
                                y = OI, 
                                fill = dispersion,
                                width = 0.4)) +
          geom_col() +
         ylim(0, 60) +
         viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "Site", y = "% cover") +
          ggtitle('(iii) Other Invertebrates') +
          theme_bw() +
          theme(plot.title = element_text(size = 12, hjust = 0), 
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none")) 
plot(oi)



# figure1 <- ggarrange(c, ta, cca, ma, 
#                      common.legend = TRUE, 
#                      legend="top",
#                      labels = c("Hard Coral", "Turf Algae", "Crustose Coralline Algae (CCA)", "Macroalgae"),
#                      font.label = list(size = 10, colour = "lightblue"),
#                      ncol=2, nrow=2
#                      )
# 
# ggsave("graphs/Figure1_trans.pdf", width = 10, height = 8)


# Figure 1 Benthic Major
ggarrange(c, fca, cca, ta, bca, ma, 
          ncol=2, nrow=3, 
          common.legend = TRUE, 
          legend="bottom")

ggsave("graphs/FigS3_major_site.pdf", width = 10, height = 12, dpi = 100)
ggsave("graphs/FigS3__major_site.tiff", width = 10, height = 12, dpi = 100)


# Figure Benthic Major - supplementary
ggarrange(sand, rub, oi, 
          ncol=1, nrow=3, 
          common.legend = TRUE, 
          legend="top")

ggsave("graphs/FigureSX_major.pdf", width = 5, height = 10, dpi = 100)
ggsave("graphs/FigureSX_major.tiff", width = 5, height = 10, dpi = 100)




## coral graphs ----
# Figure Coral genera - (MeanTotalCoral), MONTI, PORRUS, COSC, ACROP, ISOP, (PAV)

monti <- (ggplot(coral_trans, aes(x = sitename, 
                              y = MONTI, 
                              fill = dispersion,
                              width = 0.4)) +
        geom_col() +
        ylim(0, 40) +
        viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                    name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
        labs(x = "", 
             y = "% cover",
             subtitle = expression('(i)'~italic(Montipora)~'spp.')) +
        theme_bw() +
        theme(plot.title = element_text(size = 12, hjust = 0.5), 
              #subtitle = element_text(size = 12, hjust = 0),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.position = "none")) 
plot(monti)


# PORRUS
porrus <- (ggplot(coral_trans, aes(x = sitename, 
                                  y = PORRUS, 
                                  fill = dispersion,
                                  width = 0.4)) +
            geom_col() +
            ylim(0, 40) +
            viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                        name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
            labs(x = "", 
                 y = "% cover",
                 subtitle = expression('(ii)'~italic(Porites)~italic(rus)~'spp.')) +
            theme_bw() +
            theme(plot.title = element_text(size = 12, hjust = 0.5), 
                  #subtitle = element_text(size = 12, hjust = 0),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                  legend.position = "none")) 
plot(porrus)


# ACROP
acrop <- (ggplot(coral_trans, aes(x = sitename, 
                                 y = (ACROP + ACROTBL + ACROPARB + ACROST), 
                                 fill = dispersion,
                                 width = 0.4)) +
           geom_col() +
           ylim(0, 40) +
           viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                       name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
           labs(x = "", 
                y = "% cover",
                subtitle = expression('(iii)'~italic(Acropora)~'spp.')) +
           theme_bw() +
           theme(plot.title = element_text(size = 12, hjust = 0.5), 
                 #subtitle = element_text(size = 12, hjust = 0),
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                 legend.position = "none")) 
plot(acrop)

# ISOP
isop <- (ggplot(coral_trans, aes(x = sitename, 
                                  y = ISOP, 
                                  fill = dispersion,
                                  width = 0.4)) +
            geom_col() +
            ylim(0, 40) +
            viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                        name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
            labs(x = "", 
                 y = "% cover",
                 subtitle = expression('(iv)'~italic(Isopora)~'spp.')) +
            theme_bw() +
            theme(plot.title = element_text(size = 12, hjust = 0.5), 
                  #subtitle = element_text(size = 12, hjust = 0),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                  legend.position = "none")) 
plot(isop)


# PAV
pav <- (ggplot(coral_trans, aes(x = sitename, 
                                 y = PAV, 
                                 fill = dispersion,
                                 width = 0.4)) +
           geom_col() +
           ylim(0, 40) +
           viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                       name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
           labs(x = "", 
                y = "% cover",
                subtitle = expression('(v)'~italic(Pavona)~'spp.')) +
           theme_bw() +
           theme(plot.title = element_text(size = 12, hjust = 0.5), 
                 #subtitle = element_text(size = 12, hjust = 0),
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                 legend.position = "none")) 
plot(pav)


# ACROTBL
actbl <- (ggplot(coral_trans, aes(x = sitename, 
                                y = ACROTBL, 
                                fill = dispersion,
                                width = 0.4)) +
          geom_col() +
          ylim(0, 40) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "", 
               y = "% cover",
               subtitle = expression('(vi)'~italic(Acropora)~'table')) +
          theme_bw() +
          theme(plot.title = element_text(size = 12, hjust = 0.5), 
                #subtitle = element_text(size = 12, hjust = 0),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none")) 
plot(actbl)


# POC
poc <- (ggplot(coral_trans, aes(x = sitename, 
                                  y = POC, 
                                  fill = dispersion,
                                  width = 0.4)) +
            geom_col() +
            ylim(0, 40) +
            viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                        name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
            labs(x = "", 
                 y = "% cover",
                 subtitle = expression('(vii)'~italic(Pocillopora)~'spp.')) +
            theme_bw() +
            theme(plot.title = element_text(size = 12, hjust = 0.5), 
                  #subtitle = element_text(size = 12, hjust = 0),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                  legend.position = "none")) 
plot(poc)


# PORMAS
pormas <- (ggplot(coral_trans, aes(x = sitename, 
                                y = PORMAS, 
                                fill = dispersion,
                                width = 0.4)) +
          geom_col() +
          ylim(0, 40) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "", 
               y = "% cover",
               subtitle = expression('(vi)'~italic(Porites)~'massive')) +
          theme_bw() +
          theme(plot.title = element_text(size = 12, hjust = 0.5), 
                #subtitle = element_text(size = 12, hjust = 0),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none")) 
plot(pormas)


# LEPT
lept <- (ggplot(coral_trans, aes(x = sitename, 
                                   y = LEPT, 
                                   fill = dispersion,
                                   width = 0.4)) +
             geom_col() +
             ylim(0, 40) +
             viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                         name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
             labs(x = "", 
                  y = "% cover",
                  title = "",
                  subtitle = "(ix) Leptastrea spp.") +
             theme_bw() +
             theme(plot.title = element_text(size = 12, hjust = 0.5), 
                   #subtitle = element_text(size = 12, hjust = 0),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                   legend.position = "none")) 
plot(lept)


# LOBOPH
lobo <- (ggplot(coral_trans, aes(x = sitename, 
                                 y = LOBOPH, 
                                 fill = dispersion,
                                 width = 0.4)) +
           geom_col() +
           ylim(0, 40) +
           viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                       name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
           labs(x = "", 
                y = "% cover",
                title = "",
                subtitle = "(x) Lobophyllia spp.") +
           theme_bw() +
           theme(plot.title = element_text(size = 12, hjust = 0.5), 
                 #subtitle = element_text(size = 12, hjust = 0),
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                 legend.position = "none")) 
plot(lobo)



# COSC
cosc <- (ggplot(coral_trans, aes(x = sitename, 
                                   y = COSC, 
                                   fill = dispersion,
                                   width = 0.4)) +
             geom_col() +
             ylim(0, 40) +
             viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                         name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
             labs(x = "", 
                  y = "% cover",
                  title = "",
                  subtitle = "(x) Coscinarea") +
             theme_bw() +
             theme(plot.title = element_text(size = 12, hjust = 0.5), 
                   #subtitle = element_text(size = 12, hjust = 0),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                   legend.position = "none")) 
plot(cosc)


# Figure 3 Coral genera
ggarrange(monti, porrus, acrop, isop, pav, pormas, 
          ncol=2, nrow=3, 
          common.legend = TRUE, 
          legend="bottom")

ggsave("graphs/FigS4_coral_site.pdf", width = 10, height = 12, dpi = 100)
ggsave("graphs/FigS4_coral_site.tiff", width = 10, height = 12, dpi = 100)



