#https://github.com/thomazbastiaanssen/kronos/blob/main/R/plotting.R
#install.packages("kronos")
library(kronos)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(corrplot)
library(gprofiler2)
#
library(readxl)
setwd("/Users/mariareinacampos/Documents/00_MASTER/TFM/Resultats/Results_PD60_HPC")
bigdata_pd60_HPC <- read_excel("bigdata_pd60_HPC.xlsx")
bigmeta_pd60_HPC <- read_excel("bigmeta_pd60_HPC.xlsx")
#
#help(fw_kronos)
#fw_kronos
out_list_pd60_HPC = fw_kronos(x = bigdata_pd60_HPC[,-1],
                              formula = ~ Group + time(Timepoint), 
                              metadata = bigmeta_pd60_HPC, 
                              period = 24,
                              verbose = T,
                              pairwise = T)
#
# tonto <-out_list_pd60_HPC[[1]]
# tonto@input
# d <- merge(tonto@input, tonto@to_plot, by="row.names", all=TRUE)[,-1]
# d_noNA <- na.omit(d)
# dim(d) ; dim(d_noNA)
#
dd.todo <-apply(X=bigdata_pd60_HPC[,-1], MARGIN = 1, FUN=function(y){cbind(y,bigmeta_pd60_HPC)}) #equivalente al outlist pero con otra estructura
gene_names <- bigdata_pd60_HPC$Genes
plot_list <- vector(mode = "list", length = length(out_list_pd60_HPC))
plot_list2 <- vector(mode = "list", length = length(out_list_pd60_HPC))
#
for(q in 1:length(out_list_pd60_HPC)){
  plot_list[[q]] <- gg_kronos_sinusoid(out_list_pd60_HPC[[q]]) +
    scale_colour_manual(values = c("Water" = "#000000", "Ethanol" = "#F764C6")) + #line color
    scale_fill_manual(values = c("Water" = "#FFFFFF", "Ethanol" = "#F764C6")) + #filling color
    xlab("ZT") +
    ylab("Fold change") +
    ggtitle(paste(gene_names[q], "expression", sep = " "))
  dd <- dd.todo[[q]]
  dd$Timepoint <- factor(dd$Timepoint)
  plot_list2[[q]]<- ggplot(dd,aes(Timepoint,y))+geom_boxplot(aes(fill=Group))+
    ylab("Fold change")+
    scale_fill_manual(values = c("Water" = "#FFFFFF", "Ethanol" = "#F764C6"))+
    ggtitle(paste(gene_names[q], "expression", sep = " "))
  }
#
pdf("Plots_doble.pdf")
for (i in 1:length(plot_list)) {
  grid.arrange(plot_list[[i]],plot_list2[[i]],nrow=2)
}
dev.off()
#
# warnings()