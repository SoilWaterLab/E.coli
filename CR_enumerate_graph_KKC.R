#Created by Keiran Cantilina with help from Allison Truhlar
#Last updated by Keiran Cantilina on July 16, 2015

#Clean Slate
rm(list=ls())


#Start needed libraries
library(plyr)
library(ggplot2)
library(reshape2)


#Data accession and assignment of classes
fulldata <- read.csv ("C://Users//Ross//Desktop//R_code//Trial2_CRmorphotypeID_ModifiedKKC.csv")
setwd("C://Users//Ross//Desktop//R_code//")
dir <- getwd()
data <- na.omit(fulldata)
data$Pot <- factor(data$Pot)
data$Week <- factor(data$Week)
data$CRScore <- factor(data$CRScore)


#Creates Column "Treatment" and assigns values by pot
i <- 1 #Index
for (i in 1:nrow(data)){
  if(data$Pot[i]==1|data$Pot[i]==2|data$Pot[i]==15|data$Pot[i]==16){
  data$Treatment[i] <- "Control"
  } else if(data$Pot[i]==3|data$Pot[i]==4|data$Pot[i]==17|data$Pot[i]==18){
    data$Treatment[i] <- "Manure, SA"
  } else if(data$Pot[i]==5|data$Pot[i]==6|data$Pot[i]==19|data$Pot[i]==20){
    data$Treatment[i] <- "Manure, incorp"
  } else if(data$Pot[i]==7|data$Pot[i]==8|data$Pot[i]==21|data$Pot[i]==22){
    data$Treatment[i] <- "Inoc soil, SA"
  } else if(data$Pot[i]==9|data$Pot[i]==10|data$Pot[i]==23|data$Pot[i]==24){
    data$Treatment[i] <- "Inoc soil, incorp"
  } else if(data$Pot[i]==11|data$Pot[i]==12|data$Pot[i]==25|data$Pot[i]==26){
    data$Treatment[i] <- "Inoc manure, SA"
  } else if(data$Pot[i]==13|data$Pot[i]==14|data$Pot[i]==27|data$Pot[i]==28){
    data$Treatment[i] <- "Inoc manure, Incorp"
  }
  i <- i+1} #Index moves up by one before loop ends


#Identify 5s
j <- 1 #Index
for (i in 1:nrow(data)){
  if(data$CRScore[j]==5){
  data$Fives[j] <- 1
  } 
  else {data$Fives[j] <- 0
  }
  j <- j+1}


#Collapse columns with identical Pot and Week numbers to mean 5s, then outputs as dataframe "CRData"
CRData <- ddply(data, .(Week, Pot, Treatment), summarize, Prop5 = mean(Fives))
#Collapse for treatments and find min and max
CRData <- ddply(CRData, .(Week, Treatment), summarize, max5 = max(Prop5), min5 = min(Prop5), Prop5 = mean(Prop5))
#Make row index
CRData$Index <- 1:nrow(CRData)
#Change column order
CRData <- CRData[c(6,1,2,5,3,4)]
#Write to file
write.csv(CRData, file ="C://Users//Ross//Desktop//R_code//CRData_Trial 2.csv")


#Create error bar limits with min and max
limits <- aes(ymax=CRData$max5, ymin=CRData$min5)

# Scatterplot
pd <- position_dodge(0.1)

CR_plot <- ggplot(CRData, aes(x=Week,y=Prop5, colour=Treatment, group=Treatment))+
  geom_errorbar(limits, width = 0, colour="black",position=pd)+
  geom_line(colour="black",position=pd)+
  geom_point(size=4,position=pd)+
  #scale_shape_manual(values=c(15,15,17,17,19,19))+
  #scale_color_manual(colours=rainbow(8))+
  #scale_x_continuous(breaks=c(0,7,14,21))+
  ylab("Proportion C+")+
  xlab("Week")+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=14),
        legend.title=element_text(size=14,face="bold"))
CR_plot + ggtitle("Curli Trends Over Time") + theme(plot.title = element_text(size=18, face="bold"))
graph_file <- paste(dir,"/Trial2_CR.png",sep="")
ggsave(file=graph_file, plot=CR_plot)


#Stats stuff(Not done yet; I have no idea how this works)
CRData$Treatment <- factor(CRData$Treatment)
model <- aov(Prop5~Treatment*Week, data=CRData)
summary(model)
TukeyHSD(model)
