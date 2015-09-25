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
curve_list <- as.data.frame(curve_list[c(5,6,1,2,3,4),])
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
colnames(LB1_list) <- "File"
LB0.1_list <- data.frame(list.files(path=dir, pattern="*_LB0.1.csv*"))
colnames(LB0.1_list) <- "File"
LB0.05_list <- data.frame(list.files(path=dir, pattern="*_LB0.05.csv*"))
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

#NOTE: At this point, each column is a time point. The rows are isolates. I dunno why.
j<-1
while (j<7){
  
  a <- mean_se(old_soil_SA[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
old_soil_SA_stats <- b


j<-1
while (j<7){
  
  a <- mean_se(old_manure_SA[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
old_manure_SA_stats <- b

j<-1
while (j<7){
  
  a <- mean_se(old_soil_incorp[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
old_soil_incorp_stats <- b


j<-1
while (j<7){
  
  a <- mean_se(old_manure_incorp[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
old_manure_incorp_stats <- b

j<-1
while (j<7){
  
  a <- mean_se(old_red[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
old_red_stats <- b

j<-1
while (j<7){
  
  a <- mean_se(old_white[,j])
  if(j==1){
    b<-a
  }
  if(j>1){
    b<- rbind(b,a)
  }
  j<-j+1
}
old_white_stats <- b

#NOTE TO ALLISON: I ran out of time to finish
#this, but the only thing left to do is to plot. Use the *_stats
#dataframes with ggplot and it should work. See you tomorrow!




