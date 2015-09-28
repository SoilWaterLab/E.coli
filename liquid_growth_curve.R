#ALLLL of the spagetti code. My apologies for anyone that has to work
#with this; I wrote it in a merry rush and so it's pretty messy and not at
#all efficient. 

#A big chunk of this code can be recycled for reading from multiple compound
#plate reader output files. 


# Clear workspace
rm(list=ls())


#Load libraries if just starting
library(reshape2)
library(plyr)
library(ggplot2)
library(TTR)
library(zoo)


#Set working R directory
#Keiran: "C://Users//Ross//Desktop//R_code//GC//"
dir <- "C://Users//Ross//Desktop//R_code//Ecoli_project//liquid_growth_curve//"
setwd(dir)

#Open files
curve_list <- data.frame(list.files(path=dir, pattern="*hr"))
curve_list <- as.data.frame(curve_list[c(13,14,4,8,9,10,11,12,1,2,3,5,6,7),])
colnames(curve_list) <- "File"

i=1
while (i<((nrow(curve_list))+1)){
  filename <- paste("data_",i,sep="")
  f <- read.csv(paste(dir,curve_list$File[i],sep=""))
  #assign(filename,f)
  LB0.05 <- f[2:9,2:13]
  LB0.1 <- f[13:20,2:13]
  LB1 <- f[24:31,2:13]
  write.csv(LB1, file=paste(dir,filename,"_","LB1",".csv",sep=""),row.names=FALSE)
  write.csv(LB0.1, file=paste(dir,filename,"_","LB0.1",".csv",sep=""),row.names=FALSE)
  write.csv(LB0.05, file=paste(dir,filename,"_","LB0.05",".csv",sep=""),row.names=FALSE)
  
  i<- i+1
}
i<-1
LB1_list <- data.frame(list.files(path=dir, pattern="*_LB1.csv*"))
LB1_list <- as.data.frame(LB1_list[c(6:14,1:5),])
colnames(LB1_list) <- "File"
LB0.1_list <- data.frame(list.files(path=dir, pattern="*_LB0.1.csv*"))
LB0.1_list <- as.data.frame(LB0.1_list[c(6:14,1:5),])
colnames(LB0.1_list) <- "File"
LB0.05_list <- data.frame(list.files(path=dir, pattern="*_LB0.05.csv*"))
LB0.05_list <- as.data.frame(LB0.05_list[c(6:14,1:5),])
colnames(LB0.05_list) <- "File"

while (i<((nrow(LB1_list))+1)){
  potato <- read.csv(paste(dir,LB1_list$File[i],sep=""))
  red <- potato[1,1:3]
  red <- as.data.frame(rollapply(as.numeric(red), 3, mean, by = 3, partial = FALSE))
  colnames(red) <- paste("Time",i,sep="")
  white <- potato[1,4:6]
  white <- as.data.frame(rollapply((as.numeric(white)), 3, mean, by = 3, partial = FALSE))
  colnames(white) <- paste("Time",i,sep="")
  wk0 <- as.numeric(cbind(potato[1,7:12],(potato[2,1:12])))
  wk0 <- as.data.frame(rollapply(as.numeric(wk0), 3, mean, by = 3, partial = FALSE))
  colnames(wk0) <- paste("Time",i,sep="")
  soil_SA <- cbind(potato[3,1:12],(potato[4,1:6]))
  soil_SA <- as.data.frame(rollapply(as.numeric(soil_SA), 3, mean, by = 3, partial = FALSE))
  colnames(soil_SA) <- paste("Time",i,sep="")
  soil_incorp <- cbind(potato[4,7:12],(potato[5,1:12]))
  soil_incorp<- as.data.frame(rollapply(as.numeric(soil_incorp), 3, mean, by = 3, partial = FALSE))
  colnames(soil_incorp) <- paste("Time",i,sep="")
  manure_SA <- cbind(potato[6,1:12],(potato[7,1:6]))
  manure_SA <- as.data.frame(rollapply(as.numeric(manure_SA), 3, mean, by = 3, partial = FALSE))
  colnames(manure_SA) <- paste("Time",i,sep="")
  manure_incorp <- cbind(potato[7,7:12],(potato[8,1:12]))
  manure_incorp <- as.data.frame(rollapply(as.numeric(manure_incorp), 3, mean, by = 3, partial = FALSE))
  colnames(manure_incorp) <- paste("Time",i,sep="")
  if (i==1){
    old_red <- red
    old_white <- white
    old_wk0 <- wk0
    old_soil_SA <- soil_SA
    old_soil_incorp <- soil_incorp
    old_manure_SA <- manure_SA
    old_manure_incorp <- manure_incorp
  }
  if (i>1){
    old_red <- cbind(old_red,red)
    old_white <- cbind(old_white,white)
    old_wk0 <- cbind(old_wk0,wk0)
    old_soil_SA <- cbind(old_soil_SA,soil_SA)
    old_soil_incorp <- cbind(old_soil_incorp,soil_incorp)
    old_manure_SA <- cbind(old_manure_SA,manure_SA)
    old_manure_incorp <- cbind(old_manure_incorp,manure_incorp)
  }
  i<-i+1
}

times <- c(0,0.5,1,2,3,4,6,8,10,12,14,21,23,27)
w <- nrow(LB1_list)+1
#NOTE: At this point, each column is a time point. The rows are isolates. I dunno why.
j<-1
while (j<w){
  
  a <- mean_se(old_soil_SA[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
b$Time <- times
b$Treatment <- "Soil SA"
old_soil_SA_stats <- b


j<-1
while (j<w){
  
  a <- mean_se(old_manure_SA[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
b$Time <- times
b$Treatment <- "Manure SA"
old_manure_SA_stats <- b

j<-1
while (j<w){
  
  a <- mean_se(old_soil_incorp[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
b$Time <- times
b$Treatment <- "Soil Incorp"
old_soil_incorp_stats <- b


j<-1
while (j<w){
  
  a <- mean_se(old_manure_incorp[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
b$Time <- times
b$Treatment <- "Manure Incorp"
old_manure_incorp_stats <- b

j<-1
while (j<w){
  
  a <- mean_se(old_red[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
b$Time <- times
b$Treatment <- "Red"
old_red_stats <- b

j<-1
while (j<w){
  
  a <- mean_se(old_white[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
b$Time <- times
b$Treatment <- "White"
old_white_stats <- b

j<-1
while (j<w){
  
  a <- mean_se(old_wk0[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
b$Time <- times
b$Treatment <- "Week 0"
old_wk0_stats <- b

all <- rbind(old_wk0_stats,old_red_stats,old_white_stats,old_manure_incorp_stats,old_manure_SA_stats,old_soil_SA_stats,old_soil_incorp_stats)
limits <- aes(ymax=all$ymax, ymin=all$ymin)
CR_growth_plot <- ggplot(all, aes(x=Time, y=y,colour=Treatment, group=Treatment, shape=Treatment))+
  geom_errorbar(limits, width = 0, colour="black")+
  geom_line(colour="black")+
  geom_point(size=4)+
  scale_shape_manual(values=c(15,15,8,17,17,19,8))+
  scale_color_manual(values=c("#1f78b4","#a6cee3","Red","#b2df8a","#33a02c","#fdae61","grey"))+
  ylab("OD")+
  xlab("Time (hr)")+
  theme_bw()+
  theme(axis.text=element_text(size=30), axis.title.x=element_text(size=30,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=30,vjust=1.2), legend.text=element_text(size=25),
        legend.title=element_text(size=25,face="bold"), legend.position=c(0.2,0.5))

png(filename=paste(dir,"liquid_curve.png",sep=""), height=1200, width=1600)
plot(CR_growth_plot)
dev.off()

