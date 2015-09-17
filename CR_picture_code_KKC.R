# CR_picture_code_KKC.R
# Created by Allison Truhlar
# Last edited 09/16/2015 by Allison Truhlar
# This script loads all the data from the imageJ-processed Congo Red pictures, decodes the identity of the 
# colonies by week collected and treatment, normalizes the colony color by the WT phl628 E. coli color, and
# plots the growth of the colonies over time

library(ggplot2)
library(plyr)

# Clear workspace
rm(list=ls())

# Set working directory
#Allison, PC:"C:\\Users\\Owner\\Dropbox\\Cornell\\Ecoli_research\\Data\\"
#Keiran: "C:\\Users\\Ross\\Desktop\\R_code\\"
dir <-"C:\\Users\\Owner\\Dropbox\\Cornell\\Ecoli_research\\Data\\Congo Red\\Randomized for realz\\"
setwd(dir)

# Find and load all the existing data -------------------

# Enter how many hours of data exist (can hard code this if desired)
# readline(prompt="Enter how many hours of photos exist (multiple of 24): ")

time <- 72

# Create loop to open all data files and append them into one dataframe.  Index starts at 24 (for 24h), and 
# ends when the index passes the "time" in hours just input by the user.  Note that this loop requires that 
# all four plate images (and it assumes four plates) have been processed.  To change the number of plates, 
# change the maximum value of j in the for loop.

i <- 24

while (i<=time){
  
  for (j in 1:4){
    
    file <- paste(dir,i,"h\\Plate",j,"\\Plate",j,"_",i,"h_results.csv",sep="")
    CR_data <- read.csv(file)
    CR_data$Plate <- rep(j,nrow(CR_data))
    CR_data$Time <- rep(i,nrow(CR_data))
    CR_data <- CR_data[c(9,8,1,2,3,4,5,6,7)]
    
    if(exists("CR_data_old")==TRUE){
      CR_data <- rbind(CR_data_old,CR_data)
    }
    
    CR_data_old <- CR_data
  }
  
  i = i + 24
}

# Decoding the CR_data file ---------------------------

# Open the file that contains the colony source and location information

decode_file <- "Randomized_CR_plan_revised.csv"
decode_info <- read.csv(paste(dir,decode_file,sep=""))

# Start decoding 

for (i in 1:nrow(decode_info)){

  # Then assign treatment type based on the source well
  if(is.na(pmatch("A",decode_info$Well[i]))==FALSE | is.na(pmatch("B",decode_info$Well[i]))==FALSE){
    decode_info$Treatment[i] <- "Inoc Soil, SA"
  } else if(is.na(pmatch("C",decode_info$Well[i]))==FALSE | is.na(pmatch("D",decode_info$Well[i]))==FALSE){
    decode_info$Treatment[i] <- "Inoc Soil, Incorp"
  } else if(is.na(pmatch("E",decode_info$Well[i]))==FALSE | is.na(pmatch("F",decode_info$Well[i]))==FALSE){
    decode_info$Treatment[i] <- "Inoc Manure, SA"
  } else if(is.na(pmatch("G",decode_info$Well[i]))==FALSE | is.na(pmatch("H",decode_info$Well[i]))==FALSE){
    decode_info$Treatment[i] <- "Inoc Manure, Incorp"
  } else if(is.na(pmatch("Positive",decode_info$Well[i]))==FALSE){
    decode_info$Treatment[i]<-"Positive control"
  }else if(is.na(pmatch("Negative",decode_info$Well[i]))==FALSE){
    decode_info$Treatment[i]<-"Negative control"
  }
}

# Continue decoding by taking the Treatment information just generated and matching it to the imageJ file, 
# based on matching Plate and Location.  Also take the Week information.
  
# The Plan: increment one row at a time through the CR_data dataframe.  Extract the Plate and Location 
# information.  Use grep to search the decode_info Plate and Location columns for matches.  Return the row
# number.  Then set the Week and Treatment values in CR_data using the row number from decode_info to extract
# the appropriate values of these variables.

# First add empty columns called "week" and "treatment" to the CR_data dataframe.  

col_names <- c("Week","Treatment")
CR_data[,col_names] <- NA

for (j in 1:nrow(CR_data)){
  
  target_plate <- CR_data$Plate[j]
  target_location <- CR_data$Location[j]
  CR_data$Treatment[j] <- decode_info$Treatment[decode_info$Plate==target_plate & decode_info$Location==target_location]
  
  if (CR_data$Treatment[j] != "Positive control" & CR_data$Treatment[j] != "Negative control"){
    CR_data$Week[j] <- decode_info$Week[decode_info$Plate==target_plate & decode_info$Location==target_location]
    
    # Change Week 0 treatments all to Week 0, since the isolates hadn't really been 
    # exposed to a treatment yet and are thus functionally all the same.
    if (CR_data$Week[j]==0) {
      CR_data$Treatment[j]<-"None" 
    }
    
  } else {
    CR_data$Week[j] <- NA
  }
}


# Goal 1: Normalize and plot the grey scale values -------------------------------------

# Grey scale values correspond roughly to how red the colonies are between the values for the negative and positive controls

# Start by summarizing by Time, Plate and Treatment to find the mean and SE of 1. the mean grey value and 2. the mode grey value for
# the negative and positive controls on any given plate at any given time

grey_mean_SE <- ddply(CR_data, c("Time","Plate","Treatment"), summarise, 
                      mean=mean(Mean), se=sd(Mean)/sqrt(length(Mean)))

# Using the same matching scheme as before, normalize the mean grey value of the colonies by the mean of the
# mean grey value of the positive and negative controls for a given plate and time.  Specifically, subtract the
# positive control mean and divide by the negative control mean.

CR_data[,"Normalized_Mean"] <- NA

for (j in 1:nrow(CR_data)){
  
  if (CR_data$Treatment[j] != "Positive control" & CR_data$Treatment[j] != "Negative control"){
  
  target_plate <- CR_data$Plate[j]
  target_time <- CR_data$Time[j]
  pos_mean <- grey_mean_SE$mean[grey_mean_SE$Plate == target_plate & grey_mean_SE$Time == target_time & grey_mean_SE$Treatment == "Positive control"]
  neg_mean <- grey_mean_SE$mean[grey_mean_SE$Plate == target_plate & grey_mean_SE$Time == target_time & grey_mean_SE$Treatment == "Negative control"]
  CR_data$Normalized_Mean[j] <- (CR_data$Mean[j] - pos_mean)/(neg_mean-pos_mean)
  
  } else {
    CR_data$Normalized_Mean[j] <- NA
  }
}

# Boxplots

# First remove the control data

CR_plot_data <- CR_data[CR_data$Treatment != "Positive control" & CR_data$Treatment != "Negative control",]

# # 24 hour plot
# 
# norm_scale_24 <- ggplot(CR_plot_data[CR_plot_data$Time==24,], aes(x=Week, y=Normalized_Mean, fill=interaction(Week,Treatment)))+
#   geom_boxplot()+
#   ylab("Normalized grey scale")+
#   xlab("Week collected")+
#   scale_fill_manual(values=c("#2c7bb6","#abd9e9","#e66101","#fdae61","#ffffbf"))+
#   geom_hline(yintercept=0, linetype=2, size=0.75, color="red")+
#   geom_hline(yintercept=1, linetype=2, size=0.75, color="gray")+
#   coord_cartesian(ylim=c(-2.25,2.25))+
#   theme_bw()+
#   theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
#         axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
#         legend.title=element_text(size=18,face="bold"), legend.position="right")
# norm_scale_24
# norm_scale_24_file <- paste(dir,"norm_scale_24.png",sep="")
# ggsave(file=norm_scale_24_file, plot=norm_scale_24)
# 
# # 48 hour plot
# 
# norm_scale_48 <- ggplot(CR_plot_data[CR_plot_data$Time==48,], aes(x=Week, y=Normalized_Mean, fill=interaction(Week,Treatment)))+
#   geom_boxplot()+
#   ylab("Normalized grey scale")+
#   xlab("Week collected")+
#   scale_fill_manual(values=c("#2c7bb6","#abd9e9","#e66101","#fdae61","#ffffbf"))+
#   geom_hline(yintercept=0, linetype=2, size=0.75, color="red")+
#   geom_hline(yintercept=1, linetype=2, size=0.75, color="gray")+
#   coord_cartesian(ylim=c(-2.25,2.25))+
#   theme_bw()+
#   theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
#         axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
#         legend.title=element_text(size=18,face="bold"), legend.position="right")
# norm_scale_48
# norm_scale_48_file <- paste(dir,"norm_scale_48.png",sep="")
# ggsave(file=norm_scale_48_file, plot=norm_scale_48)
# 
# # 72 hour plot
# 
# norm_scale_72 <- ggplot(CR_plot_data[CR_plot_data$Time==72,], aes(x=Week, y=Normalized_Mean, fill=interaction(Week,Treatment)))+
#   geom_boxplot()+
#   ylab("Normalized grey scale")+
#   xlab("Week collected")+
#   scale_fill_manual(values=c("#2c7bb6","#abd9e9","#e66101","#fdae61","#ffffbf"))+
#   geom_hline(yintercept=0, linetype=2, size=0.75, color="red")+
#   geom_hline(yintercept=1, linetype=2, size=0.75, color="gray")+
#   coord_cartesian(ylim=c(-2.25,2.25))+
#   theme_bw()+
#   theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
#         axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
#         legend.title=element_text(size=18,face="bold"), legend.position="right")
# norm_scale_72
# norm_scale_72_file <- paste(dir,"norm_scale_72.png",sep="")
# ggsave(file=norm_scale_72_file, plot=norm_scale_72)

# Goal 2: Plot the area of isolates time ---------------------------------

# Create a new variable that combines Week and Treatment to form a new variable to plot by, Isolate_type.
# First add an empty column called "Isolate_Type" to the CR_data dataframe.  

# col_names <- c("Isolate_Type")
# CR_data[,col_names] <- NA

for (j in 1:nrow(CR_data)){
  CR_data$Isolate_Type[j] <- paste(CR_data$Treatment[j],"_Week",CR_data$Week[j],sep="")
}

# Now summarize by Isolate_type and Time to find the mean and SE of Area

area_mean_SE <- ddply(CR_data, c("Isolate_Type","Time"), summarise, mean=mean(Area), se=sd(Area)/sqrt(length(Area)))
area_mean_SE <- transform(area_mean_SE, lower=mean-se, upper=mean+se)
limits <- aes(ymax=area_mean_SE$upper, ymin=area_mean_SE$lower)

pd <- position_dodge(0.1)

CR_growth_plot <- ggplot(area_mean_SE, aes(x=Time, y=mean, colour=Isolate_type, shape=Isolate_type, group=Isolate_type))+
  geom_errorbar(limits, width = 0, colour="black",position=pd)+
  geom_line(colour="black",position=pd)+
  geom_point(size=4,position=pd)+
  #scale_shape_manual(values=c(15,16,17,18,19,20,21,22))+
  #scale_color_manual(values=c("#1f78b4","#a6cee3","#33a02c","#b2df8a","#1f78b4","#a6cee3","#33a02c","#b2df8a"))+
  ylab("Area (cm^2)")+
  xlab("Time (hr)")+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=10),
        legend.title=element_text(size=10,face="bold"), legend.position="right")
CR_growth_plot
plot_filename <- paste(dir,"CR_growth_plot","_all.png", sep="")
ggsave(file=plot_filename, plot=CR_growth_plot)


#Graph 2

area_mean_SE <- ddply(CR_data, c("Time","Week"), summarise, mean=mean(Area), se=sd(Area)/sqrt(length(Area)))
area_mean_SE <- transform(area_mean_SE, lower=mean-se, upper=mean+se)
limits <- aes(ymax=area_mean_SE$upper, ymin=area_mean_SE$lower)

pd <- position_dodge(0.1)

area_mean_SE <- na.omit(area_mean_SE) #Uncomment to exclude controls
area_mean_SE$Week <- as.factor(area_mean_SE$Week)
CR_growth_plot_2 <- ggplot(area_mean_SE, aes(x=Time, y=mean, colour=Week, shape=Week, group=Week))+
  geom_errorbar(limits, width = 0, colour="black",position=pd)+
  geom_line(colour="black",position=pd)+
  geom_point(size=4,position=pd)+
  #scale_shape_manual(values=c(15,16))+
  #scale_color_manual(values=c("#1f78b4","#a6cee3"))+
  ylab("Area (cm^2)")+
  xlab("Time (hr)")+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=10),
        legend.title=element_text(size=10,face="bold"), legend.position="right")
CR_growth_plot_2
plot_filename <- paste(dir,"CR_growth_plot_2","_all.png", sep="")
ggsave(file=plot_filename, plot=CR_growth_plot_2)
