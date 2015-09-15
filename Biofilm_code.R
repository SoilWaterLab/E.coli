library(ggplot2)

# Clear workspace
rm(list=ls())

# Set working directory
#Allison, PC:"C:\\Users\\Owner\\Dropbox\\Cornell\\Ecoli_research\\Data\\"
dir <-"C:\\Users\\Owner\\Dropbox\\Cornell\\Ecoli_research\\Data\\"
setwd(dir)

biofilm_file <- "Biofilm\\CrystalViolet_test_090315"
biofilm_data <- read.csv(paste(dir,biofilm_file,".csv",sep=""))

biofilm_boxplot <- ggplot(biofilm_data, aes(x=Type, y=Absorbance, fill=Aliquot))+
  geom_boxplot(width=0.8)+
  ylab("Absorbance")+
  xlab("Type")+
  scale_fill_grey(start=0.2,end=0.6)+
  theme_bw()+
  theme(axis.text=element_text(size=18), axis.title.x=element_text(size=18,face="bold",vjust=-0.4), 
        axis.title.y=element_text(size=18,vjust=1.2), legend.text=element_text(size=18),
        legend.title=element_text(size=18,face="bold"), legend.position="top")
biofilm_boxplot
biofilm_boxplot_file <- paste(dir,biofilm_file,"_combined.png",sep="")
ggsave(file=biofilm_boxplot_file, plot=biofilm_boxplot)

model <- lm(biofilm_data$Absorbance ~ biofilm_data$Type)
summary(model)
