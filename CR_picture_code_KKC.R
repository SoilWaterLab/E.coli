library(ggplot2)
library(plyr)

# Clear workspace
rm(list=ls())

# Set working directory
#Allison, PC:"C:\\Users\\Owner\\Dropbox\\Cornell\\Ecoli_research\\Data\\"
dir <-"C:\\Users\\Ross\\Desktop\\R_code\\"
setwd(dir)

# Open the file with all the imageJ data

CR_file <- "Randomized-test-data.csv"
CR_data <- read.csv(paste(dir,CR_file,sep=""))

# Open the file that contains the colony source and location information

decode_file <- "Randomized_CR_plan_revised.csv"
decode_info <- read.csv(paste(dir,decode_file,sep=""))

# Start decoding 

for (i in 1:nrow(decode_info)){
  
  # First assign plate numbers between 1 and 2 and location numbers between 1 and 36
  # These numbers are only relevant to the test run, and will change for the actual trial
  if (decode_info$Raw_Location[i] > 36){
    decode_info$Plate[i] <- 2
    decode_info$Location[i] <- decode_info$Raw_Location[i]-36
  } else if(decode_info$Raw_Location[i]<=36){
    decode_info$Plate[i] <- 1
    decode_info$Location[i] <- decode_info$Raw_Location[i]
  }
  
  # Then assign treatment type based on the source well
  if(is.na(pmatch("A",decode_info$Well[i]))==FALSE | is.na(pmatch("B",decode_info$Well[i]))==FALSE){
    decode_info$Treatment[i] <- "Inoc Soil, SA"
  } else if(is.na(pmatch("C",decode_info$Well[i]))==FALSE | is.na(pmatch("D",decode_info$Well[i]))==FALSE){
    decode_info$Treatment[i] <- "Inoc Soil, Incorp"
  } else if(is.na(pmatch("E",decode_info$Well[i]))==FALSE | is.na(pmatch("F",decode_info$Well[i]))==FALSE){
    decode_info$Treatment[i] <- "Inoc Manure, SA"
  } else if(is.na(pmatch("G",decode_info$Well[i]))==FALSE | is.na(pmatch("H",decode_info$Well[i]))==FALSE){
    decode_info$Treatment[i] <- "Inoc Manure, Incorp"
  }
}

# Continue decoding by taking the Treatment information just generated and matching it to the imageJ file, 
# based on matching Plate and Location.  Also take the Week information.

# The Plan: increment one row at a time through the CR_data dataframe.  Within this, increment one row at a time
# through the decode_info dataframe.  Query whether the Plate and Location information in the current row of the 
# decode_info dataframe match the Plate and Location information in the current row of the CR_data frame.  If so,
# set the Treatment column of the CR_data dataframe to equal the value in the decode_info dataframe.  If not,
# allow the loop to increment to the next row of the decode_info dataframe and continue the search.

# First add empty columns called "week" and "treatment" to the CR_data dataframe

col_names <- c("Week","Treatment","Isolate_type")
CR_data[,col_names] <- NA

# for (j in 1:nrow(CR_data)){
#   for (k in 1:nrow(decode_info)){
#     if (CR_data$Plate[j] == decode_info$Plate[k] & CR_data$Location[j] == decode_info$Location[k]){
#       CR_data$Treatment[j] <- decode_info$Treatment[k]
#       CR_data$Week[j] <- decode_info$Week[k]
#     } else {
#       break
#     }
#   }
# }

# The New Plan: increment one row at a time through the CR_data dataframe.  Extract the Plate and Location 
# information.  Use grep to search the decode_info Plate and Location columns for matches.  Return the row
# number.  Then set the Week and Treatment values in CR_data using the row number from decode_info to extract
# the appropriate values of these variables.

# Note I'm starting the incrementing at 37 to skip over the control plates

for (j in 37:nrow(CR_data)){
  target_plate <- CR_data$Plate[j]
  target_location <- CR_data$Location[j]
  plate_row_match <- which(decode_info$Plate == target_plate)
  location_row_match <- which(decode_info$Location == target_location)
  match <- intersect(plate_row_match,location_row_match)
  CR_data$Week[j] <- decode_info$Week[match]
  CR_data$Treatment[j] <- decode_info$Treatment[match]
}

# I am also creating a new variable that combines Week and Treatment to form a new variable to plot by, Isolate_type.
for (j in 37:nrow(CR_data)){
  CR_data$Isolate_type[j] <- paste(CR_data$Treatment[j],"_Week",CR_data$Week[j],sep="")
}
levels(CR_data$Isolate_type)<-

# Success!  Now summarize by Isolate_type and Time to find the mean and SE of Area

area_mean_SE <- ddply(CR_data, c("Isolate_type","Time"), summarise, mean=mean(Area), se=sd(Area)/sqrt(length(Area)))
area_mean_SE <- transform(area_mean_SE, lower=mean-se, upper=mean+se)
limits <- aes(ymax=area_mean_SE$upper, ymin=area_mean_SE$lower)

pd <- position_dodge(0.1)

area_mean_SE <- na.omit(area_mean_SE)

CR_growth_plot <- ggplot(area_mean_SE, aes(x=Time, y=mean, colour=Isolate_type, shape=Isolate_type, group=Isolate_type))+
  geom_errorbar(limits, width = 0, colour="black",position=pd)+
  geom_line(colour="black",position=pd)+
  geom_point(size=4,position=pd)+
  scale_shape_manual(values=c(15,16,17,18,19,20,21,22))+
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

# biofilm_boxplot_file <- paste(dir,biofilm_file,"_combined.png",sep="")
# ggsave(file=biofilm_boxplot_file, plot=biofilm_boxplot)
# 
# model <- lm(biofilm_data$Absorbance ~ biofilm_data$Type)
# summary(model)
