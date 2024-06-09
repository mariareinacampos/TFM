library(kronos)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(corrplot)
library(gprofiler2)
library(outliers)
library(dplyr)
library(readxl)

setwd("/Users/mariareinacampos/Documents/00_MASTER/TFM/Resultats/Results_PD35_hpc")
bigdata_pd35_hpc <- read_excel("/Users/mariareinacampos/Documents/00_MASTER/TFM/Resultats/Results_PD35_hpc/bigdata_pd35_hpc_acrogram.xlsx") 
bigmeta_pd35_hpc <- read_excel("/Users/mariareinacampos/Documents/00_MASTER/TFM/Resultats/Results_PD35_hpc/bigmeta_pd35_hpc.xlsx")

out_list_pd35_hpc = fw_kronos(x = bigdata_pd35_hpc[,-1],
                              formula = ~ Group + time(Timepoint), 
                              metadata = bigmeta_pd35_hpc, 
                              period = 24,
                              verbose = F,
                              pairwise = T)


fit_df <- kronosListToTable(out_list_pd35_hpc)
fit_df <- as.data.frame(fit_df)
fit_df$Genes <- bigdata_pd35_hpc$Genes
fit_df %>% select(Genes, everything(fit_df)) #Aquí m'ho esta ensenyant a la pantalla, no és real. La funció select és per seleccionar columnes.
fit_df <- fit_df %>% select(Genes, everything(fit_df)) #Aquí estem guardant-ho de veritat
fit_df_pd35_hpc_sinusoid <- fit_df %>%  filter(Ethanol_p.val < 0.05 & Water_p.val <0.05)
print(fit_df_pd35_hpc_sinusoid)


output_folder <- "/Users/mariareinacampos/Documents/00_MASTER/TFM/Resultats/Results_PD35_hpc/Graphs_PD35_hpc"

plot <- gg_kronos_acrogram(out_list_pd35_hpc) +
  scale_fill_manual(values = c("Water" = "#A1A1A1",
                               "Ethanol" = "#F764C6")) +
  ggtitle("Acrogram PD35 hpc")

file_name <- paste0(output_folder, "/acrogram_pd35_hpc_sinusoid.png")

ggsave(file = file_name, plot = plot, width = 6, height = 4, units = "in")
