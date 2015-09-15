#Created by Keiran Cantilina
#Last updated by Keiran Cantilina on July 25, 2015

#Clean Slate
rm(list=ls())

#Turn off warnings because for whatever reason wilcox produces tons
options(warn=-1, echo=FALSE)


#Start needed libraries
library(plyr)
library(ggplot2)
library(reshape2)

#Data accession and assignment of classes
#Allison: 
fulldata <- read.csv ("C://Users//Owner//Dropbox//Cornell//Ecoli_research//Data//R_code//Week5_enumeration_statsdata.csv")
setwd("C://Users//Owner//Dropbox//Cornell//Ecoli_research//Data//R_code")
dir <- getwd()
data <- na.omit(fulldata) #Omits NAs
data <- subset(data, select=c(Index, Treatment, CFU_ml)) #Extracts only needed columns
data$Index <- factor(data$Index)
data$Treatment <- factor(data$Treatment)


#Histogram of CFU_ml for evaluating adherence to normal curve
histogram <- hist(data$CFU_ml, breaks=100)


#Divide data into two groups using reshape2 (unmelt)
data <- dcast(data, Index~Treatment)


#Omit NAs
data <- na.omit(data)


#Wilcoxon test (non-parametric T-test)
invisible(test <- wilcox.test(data$Cycloheximide, data$Kan_Chlor, paired= TRUE))
p.value <- test$p.value
V <- test$statistic

#Display
if(p.value>0.05) answer <- ("POPULATIONS ARE NOT SIGNIFICANTLY DIFFERENT")
if(p.value<=0.05) answer <-("POPULATIONS ARE SIGNIFICANTLY DIFFERENT")
print(answer)
