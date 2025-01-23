## 4. Graphics
## g) Correlation Matrix (fig S1 & S2)
## Description: Biotic and Abiotic correlation matrix 
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

# remove sitename column 
Predictors <- select(Predictors, -1)                  


# create correlation matrix
correlation_matrix <- round(cor(Predictors),1)

ggcorrplot(correlation_matrix, method ="circle")

corrp.mat <- cor_pmat(Predictors)

correlations <- ggcorrplot(correlation_matrix, hc.order =TRUE, 
           type ="lower", lab =TRUE, outline.color ="white",lab_size = 6, tl.cex = 20)

correlations


ggsave("graphs/FigS1_correlations_hali.png", plot = correlations, width = 30, height = 30, units = "cm", dpi = 100)
ggsave("graphs/FigS1_correlations_hali.pdf", plot = correlations, width = 30, height = 30, units = "cm", dpi = 100)
ggsave("graphs/FigS1_correlations_hali.tiff", plot = correlations, width = 30, height = 30, units = "cm", dpi = 100)



## Environmental Predictor variable correlations ----

Predictors_env <-read.csv("data/abiotic_dec2024.csv")

str(Predictors_env)

# remove sitename column ï..sitename
Predictors_env <- select(Predictors_env, -1)                  # remove blank columns 

library(ggplot2)
library(ggcorrplot)

str(Predictors_env)

# remove "mean + 1SD" and "SE"
Predictors_env <- select(Predictors_env, -DIN_se, -DIN_mean_1SD) # remove the columns


?ggcorrplot

correlation_matrix <- round(cor(na.omit(Predictors_env)),1)

ggcorrplot(correlation_matrix, method ="circle")   # try method = "circle"

corrp.mat <- cor_pmat(Predictors_env)

correlations_env <- ggcorrplot(correlation_matrix, hc.order =TRUE, 
                           type ="lower", lab =TRUE, outline.color ="white",lab_size = 6, tl.cex = 20) +
  theme(text = element_text(size = 20),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20))

correlations_env


ggsave("graphs/FigS2_correlations_env.png", plot = correlations_env, width = 30, height = 30, units = "cm", dpi = 100)
ggsave("graphs/FigS2_correlations_env.pdf", plot = correlations_env, width = 30, height = 30, units = "cm", dpi = 100)
ggsave("graphs/FigS2_correlations_env.tiff", plot = correlations_env, width = 30, height = 30, units = "cm", dpi = 100)

