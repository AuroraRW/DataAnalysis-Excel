##################################################
### 03-Clustering:K-Means                       ## 
##################################################
#                                               ##
##################################################
# Written by Aurora
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/03")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
### Install Libraries                           ##
##################################################

#Check the library

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

##################################################
### Load the data and Transformation            ##
##################################################

# Read csv file and load Reviews data
Reviews_RW <- read.csv("Reviews.csv", header = TRUE, sep = ",")
str(Reviews_RW)

#Normalization function, yield data with N(0,1) distrubition
normn_RW <- function(x_RW) {
  return ((x_RW-mean(x_RW))/sd(x_RW))
}


#Normaliz data of Religious and Nature with N(0,1) distrubition
Reviews_RW$SportsNorm_RW <- normn_RW(Reviews_RW$Sports)
Reviews_RW$ReligiousNorm_RW <- normn_RW(Reviews_RW$Religious)
Reviews_RW$NatureNorm_RW <- normn_RW(Reviews_RW$Nature)
Reviews_RW$TheatreNorm_RW <- normn_RW(Reviews_RW$Theatre)
Reviews_RW$ShoppingNorm_RW <- normn_RW(Reviews_RW$Shopping)
Reviews_RW$PicnicNorm_RW <- normn_RW(Reviews_RW$Picnic)
Reviews_RW$AgeNorm_RW <- normn_RW(Reviews_RW$Age)
Reviews_RW$IncomeNorm_RW <- normn_RW(Reviews_RW$Income)
Reviews_RW$NbrNorm_RW <- normn_RW(Reviews_RW$Nbr)

head(Reviews_RW)

##################################################
##Descriptive Data Analysis                     ##
##################################################

#Summaries of data
summary(Reviews_RW)
stat.desc(Reviews_RW)
str(Reviews_RW)

# Histogram for all variables
par(mfrow=c(3,3)) 
sapply(names(Reviews_RW), function(cname_RW){
  # only plot the numeric columns
  if (is.numeric(Reviews_RW[[cname_RW]]))
    print(hist(Reviews_RW[[cname_RW]], main=cname_RW))
})
par(mfrow=c(1,1))


# BoxPlot for all variables
par(mfrow=c(3,3))
sapply(names(Reviews_RW), function(cname_RW){
  # only plot the numeric columns
  if (is.numeric(Reviews_RW[[cname_RW]]))
    print(boxplot(Reviews_RW[[cname_RW]], main=cname_RW))
})
par(mfrow=c(1,1))

#Get Religious and Nature
ReviewsClstr_RW <- Reviews_RW[c(12,13)]   
str(ReviewsClstr_RW)

###################################################
## Clustering with different K                   ##
###################################################

#k=2
ClstrRev_RW <- kmeans(ReviewsClstr_RW, iter.max=10, centers=2, nstart=10)
ClstrRev_RW
ClstrRev_RW$tot.withinss

#k=3
ClstrRev_RW <- kmeans(ReviewsClstr_RW, iter.max=10, centers=3, nstart=10)
ClstrRev_RW
ClstrRev_RW$tot.withinss

#k=4
ClstrRev_RW <- kmeans(ReviewsClstr_RW, iter.max=10, centers=4, nstart=10)
ClstrRev_RW
ClstrRev_RW$tot.withinss

#k=5
ClstrRev_RW <- kmeans(ReviewsClstr_RW, iter.max=10, centers=5, nstart=10)
ClstrRev_RW
ClstrRev_RW$tot.withinss

#k=6
ClstrRev_RW <- kmeans(ReviewsClstr_RW, iter.max=10, centers=6, nstart=10)
ClstrRev_RW
ClstrRev_RW$tot.withinss

##################################################
##Evaluation of Clusters                        ##
##################################################

#Based on 'elbow', choose k=3 
#k=3
ClstrRev_RW <- kmeans(ReviewsClstr_RW, iter.max=10, centers=3, nstart=10)
ClstrRev_RW

Reviews_RW$cluster <- factor(ClstrRev_RW$cluster)   # Adding Cluster tags to variables
head(Reviews_RW)

centers_RW <- data.frame(cluster=factor(1:3), ClstrRev_RW$centers)



#Plotting the Clusters      

ggplot(data=Reviews_RW, aes(x=ReligiousNorm_RW, y=NatureNorm_RW, color=cluster)) + geom_point()

ggplot(data=Reviews_RW, aes(x=ReligiousNorm_RW, y=NatureNorm_RW, color=cluster, shape=cluster)) + 
  geom_point(alpha=.8) +
  geom_point(data=centers_RW, aes(x=ReligiousNorm_RW, y=NatureNorm_RW), size=3, stroke=2)

#Summary table
ReviewsSUM_RW <- Reviews_RW %>% 
  group_by(cluster) %>% 
  summarise(Sports = mean(Sports), Religious = mean(Religious), Nature=mean(Nature), Theatre=mean(Theatre), 
            Shopping=mean(Shopping), Picnic=mean(Picnic), Age=mean(Age), Income=mean(Income), Nbr=mean(Nbr), N=n() )

ReviewsSUM_RW
