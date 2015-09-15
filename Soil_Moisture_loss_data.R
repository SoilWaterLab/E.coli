# Script for plotting E. coli experiment soil moisture % loss and differences between plots

# Last edited on 07/22/15 by Selene Leung

# Clear workspace
rm(list=ls())

# Install libraryies if just starting
#install.packages("reshape2")
#install.packages("plyr")
#install.packages("ggplot2")

# Load libraries if just starting
library(reshape2)
library(plyr)
library(ggplot2)

# Set working directory
#Selene(Mac): "/Users/SeleneLeung/Dropbox/Ecoli_research/Data/"
dir <-"/Users/SeleneLeung/Dropbox/Ecoli_research/Data/"
setwd(paste(dir,sep="",collapse=NULL))


# Load soil moisture loss data 
# **Make sure spreadsheet is updated first!!!
soilmoistureloss_file <- "Trial2/Trial2_soilmoistureloss.csv"
all_soilmoistureloss_data <- read.csv(paste(dir,soilmoistureloss_file,sep=""))
soilmoistureloss_data <- all_soilmoistureloss_data
soilmoistureloss_data$Pot <- factor(soilmoistureloss_data$Pot)
soilmoistureloss_data$Week <- factor(soilmoistureloss_data$Week)

###################################################################################################################################
# Summarize soil moisture loss data by pot to plot the means of the rewetted and dried soil samples' water holding capacity by week
soilmoistureloss_melted <- melt(soilmoistureloss_data, measure.vars=c("WHC_Rewetted","WHC_Dried"))
soilmoistureloss.means.sem <- ddply(soilmoistureloss_melted, .(Week, variable), sem=sd(value, na.rm=TRUE)/sqrt(length(value)), mean=mean(value, na.rm=TRUE),summarize)
#Find min and max of soil moisture loss means 
soilmoistureloss.means.sem <- transform(soilmoistureloss.means.sem, lower=mean-sem, upper=mean+sem)
limits <- aes(ymax=soilmoistureloss.means.sem$upper, ymin=soilmoistureloss.means.sem$lower)

# Scatterplot to Obtain E.coli Experiment Soil Moisture Loss
pd <- position_dodge(0.1)
soilmoistureloss_plot <- ggplot(soilmoistureloss.means.sem, aes(x=Week,y=mean, color=variable, group=variable))+
  geom_errorbar(limits, width = 0, colour="black",position=pd)+
  geom_line(colour="black",position=pd)+
  geom_point(size=4,position=pd)+
  scale_shape_manual(values=c(15,15,17,17,19,19))+
  scale_color_manual(values=c("#1f78b4","#a6cee3","#33a02c","#b2df8a","#e31a1c","#fb9a99"))+
  #scale_x_continuous(breaks=c(0,7,14,21))+
  ggtitle("E.coli Experiment Soil Moisture Loss")+
    theme(plot.title = element_text(lineheight=0.8, face="bold"))+
  labs(x="Week",y="% Water Holding Capacity (WHC)")
soilmoistureloss_plot
graph_file <- paste(dir,"Trial2_soilmoistureloss.png",sep="")
ggsave(file=graph_file, plot=soilmoistureloss.plot)

#############################################################################################################################
# Difference (WHC_Rewetted-WHC_Dried) for all pots each week to see if any stand out
soilmoistureloss_data <- subset(soilmoistureloss_data, select=c("Pot", "Week", "Difference"))
soilmoistureloss_data <- na.omit(soilmoistureloss_data)

# Scatterplot to observe Difference 
pd <- position_dodge(0.1)
soilmoistureloss_plot <- ggplot(soilmoistureloss_data, aes(x=Week,y=Difference, colour=Pot, group=Pot))+
  geom_line(colour="black",position=pd)+
  geom_point(size=4,position=pd)+
  #scale_shape_manual(values=c(15,15,17,17,19,19))+
  scale_color_gradientn(colours=rainbow(28))+
  #scale_x_continuous(breaks=c(0,7,14,21))+
  ggtitle("WHC Difference Between Pots")+
    theme(plot.title = element_text(lineheight=0.8, face="bold"))+
  labs(x="Week",y="% Water Holding Capacity (WHC)")
soilmoistureloss_plot
graph_file <- paste(dir,"Trial2_potdifferences.png",sep="")
ggsave(file=graph_file, plot=soilmoistureloss_plot)

#################################################################################################################################
# Difference (WHC_Rewetted-WHC_Dried) for all pots each week to see if any stand out
soilmoistureloss_data <- subset(soilmoistureloss_data, select=c("Pot", "Week", "Difference", "Manure...Mixing"))
soilmoistureloss_data <- na.omit(soilmoistureloss_data)

# Scatterplot to observe Difference 
pd <- position_dodge(0.1)
soilmoistureloss_plot <- ggplot(soilmoistureloss_data, aes(x=Week,y=Difference, color=Manure...Mixing, group=Pot))+
  geom_line(colour="black",position=pd)+
  geom_point(size=4,position=pd)+
  #scale_shape_manual(values=c(15,15,17,17,19,19))+
  #scale_color_gradientn(colours=rainbow(28))+
  #scale_x_continuous(breaks=c(0,7,14,21))+
  ggtitle("WHC Difference Between Pots with Variable Manure & Mixing Treatments")+
  theme(plot.title = element_text(lineheight=0.8, face="bold"))+
  labs(x="Week",y="% Water Holding Capacity (WHC)")
soilmoistureloss_plot
graph_file <- paste(dir,"Trial2_potdifferences.png",sep="")
ggsave(file=graph_file, plot=soilmoistureloss_plot)