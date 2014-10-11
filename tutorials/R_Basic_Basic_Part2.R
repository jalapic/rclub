####Linear Regression

#setting the working directory helps reduce root directory typing
setwd("~/Dropbox/Lab/R")

#read in relevant files
GRmoms<-read.csv("GRmoms.csv")

#if no working directory is set
GRfull<-read.csv("~/Dropbox/Lab/R/GRfull.csv")

#merge files
##maternal/litter info is contained within GRmoms
##methylation info of offspring (F3ID) is contained within GRfull
merged.data <-merge(GRmoms, GRfull, by="F3ID")
write.csv(merged.data,"~/Dropbox/Lab/R/GRdata.csv")

GRdata<-read.csv("GRdata.csv")

#regression of fec --> brain
results1=lm(GRdata$brain~GRdata$fec)
results1
summary(results1)

#regression of fec, nurse, lick --> brain
results2=lm(GRdata$brain~GRdata$nurse+GRdata$lick+GRdata$fec)
results2
summary(results2)

#compare models
anova(results1, results2)

plot(GRdata$brain,GRdata$fec)
abline(results1)

# 3D Scatterplot
library(scatterplot3d)
attach(GRdata)
scatterplot3d(GRdata$fec,GRdata$nurse,GRdata$brain, main="3D Scatterplot")

#spinning 3d scatter
library(rgl)
attach(GRdata)
plot3d(Rdata$fec,GRdata$nurse,GRdata$brain, col="red", size=3)