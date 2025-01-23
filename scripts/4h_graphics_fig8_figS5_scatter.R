## 4. Graphics
## h) Scatter plots (fig 8 & S5)
## Description: Biotic and Abiotic scatter plots
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 4th July 2024 (edited 6th Jan 2025)


## load libraries ----
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(tidyverse)


## load data ----
Predictors <-read.csv("data/predictors_all_hali.csv")

str(Predictors)

           
#### BENTHIC scatter plots ####
# with regression model to calculate R value

#coral
m_coral <- lm(Dispersion ~ Coral, data = Predictors)

(s_coral <- ggplot(Predictors, 
                        aes(y = Dispersion,
                            x = Coral)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
  #stat_smooth(method = "lm", se = F) +
 # annotate ("text", x= 56, y = 18, label=(paste0("R2 = ", format(summary(m_coral)$r.squared, digits = 3)))) +
  theme(text=element_text(family="sans", size=14),
        plot.subtitle = element_text(size = 14, hjust = 0)) +
  labs(y = "Site-level multivariate dispersion",
       x = "mean % cover",
       title = "(ii) Hard coral")+
    
    stat_cor(method="pearson", aes(label = ..r.label..), label.x = 50)
                   
)


# coral_shannon
m_coral_div <- lm(Dispersion ~ Shannon_coral, data = Predictors)

(s_coral_div <- ggplot(Predictors, 
                  aes(y = Dispersion,
                      x = Shannon_coral)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
  #stat_smooth(method = "lm", se = F) +
  #annotate ("text", x= 1.8, y = 18, label=(paste0("R2 = ", format(summary(m_coral_div)$r.squared, digits = 3)))) +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = "Site-level multivariate dispersion",
       x = "Shannon diversity index",
       title = "(vi) Coral genera diversity (Shannon)") +

    stat_cor(method="pearson", aes(label = ..r.label..), label.x = 1.75)
  
)



# turf
m_turf <- lm(Dispersion ~ Turf, data = Predictors)

(s_turf <- ggplot(Predictors, 
                  aes(y = Dispersion,
                      x = Turf)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
  # stat_smooth(method = "lm", se = F) +
  # annotate ("text", x= 27, y = 18, label=(paste0("R2 = ", format(summary(m_turf)$r.squared, digits = 3)))) +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = "Site-level multivariate dispersion",
       x = "mean % cover",
       title = "(i) Turf Algae") +

    stat_cor(method="pearson", aes(label = ..r.label..), label.x = 0.5)
)



# Macroalgae - non calcified
m_ma <- lm(Dispersion ~ Macroalgae_noncalc, data = Predictors)

(s_ma <- ggplot(Predictors, 
                 aes(y = Dispersion,
                     x = Macroalgae_noncalc)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
  # stat_smooth(method = "lm", se = F) +
  # annotate ("text", x= 26, y = 18, label=(paste0("R2 = ", format(summary(m_ma)$r.squared, digits = 3)))) +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = "Site-level multivariate dispersion",
       x = "mean % cover",
       title = "(iii) Macroalgae") +

    stat_cor(method="pearson", aes(label = ..r.label..), label.x = 0)
  
  )



# Halimeda
m_hali <- lm(Dispersion ~ Halimeda, data = Predictors)

(s_hali <- ggplot(Predictors, 
                aes(y = Dispersion,
                    x = Halimeda)) +
    geom_point(
      colour = "darkgrey",
      size = 4) +
    theme_bw() +
    # stat_smooth(method = "lm", se = F) +
    # annotate ("text", x= 26, y = 18, label=(paste0("R2 = ", format(summary(m_ma)$r.squared, digits = 3)))) +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = "Site-level multivariate dispersion",
         x = "mean % cover",
         title = expression('(v)'~italic(Halimeda)~'spp.'))+
    
    stat_cor(method="pearson", aes(label = ..r.label..), label.x = 25)
)



# benthic_shannon
m_benthic_div <- lm(Dispersion ~ Shannon_benthic, data = Predictors)

(s_benthic_div <- ggplot(Predictors, 
                 aes(y = Dispersion,
                     x = Shannon_benthic)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
  # stat_smooth(method = "lm", se = F) +
  # annotate ("text", x= 1.75, y = 18, label=(paste0("R2 = ", format(summary(m_benthic_div)$r.squared, digits = 3)))) +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = "Site-level multivariate dispersion",
       x = "Shannon diversity index",
       title = "(v) Benthic group diversity (Shannon)")+

    stat_cor(method="pearson", aes(label = ..r.label..), label.x = 1)
  
)



# FCA
m_fca <- lm(Dispersion ~ FCA, data = Predictors)

(s_fca <- ggplot(Predictors, 
                        aes(y = Dispersion,
                            x = FCA)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
  # stat_smooth(method = "lm", se = F) +
  # annotate ("text", x= 48, y = 18, label=(paste0("R2 = ", format(summary(m_fca)$r.squared, digits = 1)))) +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = "Site-level multivariate dispersion",
       x = "mean % cover",
       title = "(iv) Fleshy coralline algae")+
    
    stat_cor(method="pearson", aes(label = ..r.label..), label.x = 45)
  

)



# arrange graphs together
install.packages("ggpubr")
library(ggpubr)


ggarrange(s_turf + rremove("xlab") + rremove("ylab"),
          s_coral + rremove("xlab") + rremove("ylab"), 
          s_ma + rremove("xlab"),
          s_fca + rremove("ylab"),
          s_benthic_div + rremove("xlab") + rremove("ylab"),
          s_coral_div + rremove("ylab"), 
          ncol = 2, nrow = 3,  align = "hv")

ggsave("graphs/Figure8_scatter_benthic.pdf", width = 10, height = 12, dpi = 100)
ggsave("graphs/Figure8_scatter_benthic.tiff", width = 10, height = 12, dpi = 100)
ggsave("graphs/Figure8_scatter_benthic.png", width = 10, height = 12 ,dpi = 100)




#### ABIOTIC Predictor variable correlations ####

Predictors_env <-read.csv("data/abiotic_dec2024.csv")

str(Predictors_env)


#### ABIOTIC scatter plots ####

abiotic <-read.csv("data/abiotic_Dec2024.csv")

str(abiotic)

# need to convert to factors? ************************************************************



#population
m_pop <- lm(Dispersion ~ Human_population, data = abiotic)

(s_pop <- ggplot(abiotic, 
                  aes(y = Dispersion,
                      x = Human_population)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = "Dispersion (mean distance)",
       x = "Human population / watershed area",
       title = "(iii) Human population density")+
    stat_cor(method="pearson", aes(label = ..r.label..), label.x = 600)
  
)


# complexity

m_complex <- lm(Dispersion ~ Complexity, data = abiotic)

(s_complex <- ggplot(abiotic, 
                      aes(y = Dispersion,
                          x = Complexity)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = "Dispersion (mean distance)",
       x = "Complexity index",
       title = "(i) Habitat complexity")+
  stat_cor(method="pearson", aes(label = ..r.label..), label.x = 3.2)
  

)


# steepness

m_steep <- lm(Dispersion ~ Steepness, data = abiotic)

(s_steep <- ggplot(abiotic, 
                    aes(y = Dispersion,
                        x = Steepness)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = " ",
       x = "Steepness index",
       title = "(ii) Reef steepness")+
  stat_cor(method="pearson", aes(label = ..r.label..), label.x = 3.6)
  

)


# wave.mean
m_wave <- lm(Dispersion ~ Wave_mean, data = abiotic)

(s_wave <- ggplot(abiotic, 
                  aes(y = Dispersion,
                      x = Wave_mean)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = " ",
       x = "Mean wave power (kW/m)",
       title = "(iv) Wave power")+
    stat_cor(method="pearson", aes(label = ..r.label..), label.x = 1700)
  
)


# try without sites <1500 Kw/m
abiotic_lowwave <- subset(abiotic, wave.mean < 600)

m_wave_low <- lm(Dispersion ~ wave.mean, data = abiotic_lowwave)

s_wave_low <- ggplot(abiotic_lowwave, 
                 aes(y = Dispersion,
                     x = wave.mean)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
  stat_smooth(method = "lm", se = F) +
  annotate ("text", x= 400, y = 18, label=(paste0("R2 = ", format(summary(m_wave_low)$r.squared, digits = 3)))) +
  labs(y = " ",
       x = "Mean wave power (kW/m)",
       title = "Wave power")

plot(s_wave_low)



# mean.st.din
m_mean_din <- lm(Dispersion ~ DIN_mean, data = abiotic)

(s_mean_din <- ggplot(abiotic, 
                 aes(y = Dispersion,
                     x = DIN_mean)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
    theme(text=element_text(family="sans", size=13)) +
    labs(y = " ",
       x = "mean DIN (mg/l)",
       title = "(v) Dissolved Inorganic Nitrogen (DIN) - mean")+
  stat_cor(method="pearson", aes(label = ..r.label..), label.x = 0.17)
  
)


# max.st.din
m_max.st.din <- lm(Dispersion ~ DIN_max, data = abiotic)

(s_max.st.din <- ggplot(abiotic, 
                     aes(y = Dispersion,
                         x = DIN_max)) +
  geom_point(
    colour = "darkgrey",
    size = 4) +
  theme_bw() +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = " ",
       x = "mean DIN (mg/l)",
       title = "(v) Dissolved inorganic nitrogen (DIN) - maximum")+
  stat_cor(method="pearson", aes(label = ..r.label..), label.x = 0.22)
  
)


# disturbed_by_totalarea
m_dist <- lm(Dispersion ~ Disturbed_land, data = abiotic)

(s_dist <- ggplot(abiotic, 
                       aes(y = Dispersion,
                           x = Disturbed_land)) +
  geom_point(
    colour = "grey",
    size = 4) +
  theme_bw() +
    theme(text=element_text(family="sans", size=14)) +
    labs(y = "Dispersion (mean distance)",
       x = "Area (square Km)",
       title = "(vi) Disturbed land")+
  stat_cor(method="pearson", aes(label = ..r.label..), label.x = 0.65)
  
)


# arrange graphs together
install.packages("ggpubr")
library(ggpubr)

ggarrange(s_complex, s_steep, s_pop, s_wave, s_mean_din, s_dist,
          ncol=2, nrow=3, 
          common.legend = TRUE, 
          legend="bottom")

ggsave("graphs/FigureS5_abiotic.pdf", width = 10, height = 12, dpi = 100)
ggsave("graphs/FigureS5_abiotic.tiff", width = 10, height = 12, dpi = 100)


