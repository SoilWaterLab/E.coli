# Script for plotting E. coli enumerations

# Last edited on 05/01/15 by Allison Truhlar

# Clear workspace
rm(list=ls())

# Load libraries if just starting
library(reshape2)
library(plyr)
library(ggplot2)

# Set working directory
#Allison: "C:\\Users\\OWner\\Dropbox\\Cornell\\Ecoli_research\\Data\\"
dir <- "C://Users//Ross//Desktop//R_code"
setwd(dir)

# Load enumeration data 
# **Make sure spreadsheet is updated first!!!
enum_file <- "\\Nutrient_enumeration.csv"
all_enum_data <- read.csv(paste(dir,enum_file,sep=""))
enum_data <- na.omit(all_enum_data)
enum_data$Plate <- factor(enum_data$Plate)
enum_data$Set <- factor(enum_data$Set)


# Summarise enumerations data by pot
enum.means.sem <- ddply(enum_data, c("Day","Plate"), summarise, mean=mean(log_CFU_ml), sem=sd(log_CFU_ml)/sqrt(length(log_CFU_ml)))
enum.means.sem <- transform(enum.means.sem, lower=mean-sem, upper=mean+sem)
limits <- aes(ymax=enum.means.sem$upper, ymin=enum.means.sem$lower)

# Scatterplot
pd <- position_dodge(0.1)

enum_plot <- ggplot(enum.means.sem, aes(x=Day,y=mean, colour=Plate, shape=Plate, group=Plate))+
  geom_errorbar(limits, width = 0, colour="black",position=pd)+
  geom_line(colour="black",position=pd)+
  geom_point(size=4,position=pd)+
  scale_shape_manual(values=c(15,15,17,17,19,19))+
  scale_color_manual(values=c("#1f78b4","#a6cee3","#33a02c","#b2df8a","#e31a1c","#fb9a99"))+
  scale_x_continuous(breaks=c(0,5))+
  ylab("log(cfu/ml)")+
  xlab("Day")+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=14),
        legend.title=element_text(size=14,face="bold"))
enum_plot
graph_file <- paste(dir,"//Nutrient_enumerations.png",sep="")
ggsave(file=graph_file, plot=enum_plot)

