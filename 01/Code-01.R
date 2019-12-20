##################################################
### 01-Multivariate Linear Regression           ##
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
setwd("C:/01")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(lattice)){install.packages("lattice")}
library("lattice")

if(!require(HSAUR)){install.packages("HSAUR")}
library("HSAUR")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

##################################################
### Read in Data                                ##
##################################################

##################################################
### Read data and do preliminary data checks    ##
##################################################

# Read text file (".txt")
# Diamond price sheet
Diamond_RW <- read.delim("diamondss.txt", header=TRUE)

#Prints data 5 to 15 to make sure it looks correct
Diamond_RW[5:15,]     

ls(Diamond_RW)      #Lists all objects for Diamond price Data
names(Diamond_RW)   #List the variables for Diamond price Data
str(Diamond_RW)     #List structure of Diamond price Data

###################################################
## Preliminary data transformation               ##
###################################################
#Convert Price, Clarity, Color, Cut, Year into numeric
Diamond_RW$Price<-as.numeric(Diamond_RW$Price)
Diamond_RW$Clarity<-as.numeric(Diamond_RW$Clarity)
Diamond_RW$Color<-as.numeric(Diamond_RW$Color)
Diamond_RW$Cut<-as.numeric(Diamond_RW$Cut)
Diamond_RW$Year<-as.numeric(Diamond_RW$Year)

#Convert Source to index (Dummy) Variables
SouDummy_RW <- model.matrix(~Source -1, data=Diamond_RW)
head(SouDummy_RW)

#Combine the Datasets again
Diamond_RW <- cbind(Diamond_RW, SouDummy_RW)
head(Diamond_RW)

str(Diamond_RW)   #Check Results

#Adjust Names Again
names(Diamond_RW) <- c("Price", "Carat", "Clarity", "Color", "Cut", 
                   "Source","Year","SourceAlrosa","SourceDeBeers",
                   "SourceDebswana","SourceRioTinto")
head(Diamond_RW)  #Check Results

###################################################
## Univariate Descriptive Analysis               ##
###################################################

summary(Diamond_RW)

Diamond_RW <- Diamond_RW[-c(6)]  #Drop un-needed column

par(mfrow=c(2,3))#Fit more graphs in!

# Histogram for all variables
# loop over column *names* instead of actual columns
sapply(names(Diamond_RW), function(cn_RW){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Diamond_RW[[cn_RW]]))
    # use the `main` param to put column name as plot title
    print(hist(Diamond_RW[[cn_RW]], main=cn_RW))
})

par(mfrow=c(1,1))

###################################################
## Find Outliers                                 ##
###################################################

par(mfrow=c(2,3))

# BoxPlot for all variables
# loop over column *names* instead of actual columns
sapply(names(Diamond_RW), function(cn_RW){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Diamond_RW[[cn_RW]]))
    # use the `main` param to put column name as plot title
    print(boxplot(Diamond_RW[[cn_RW]], main=cn_RW))
})

par(mfrow=c(1,1))

#########################################
## Test Data for Normalicy             ##
#########################################

### NOTE - Instead of doing these one at a time, do them all together.

DiaNrm_RW <- lapply(Diamond_RW, shapiro.test)
DiaNrm_RW
str(DiaNrm_RW[[4]])

DiaRes_RW <- sapply(DiaNrm_RW, `[`, c("statistic","p.value"))
DiaRes_RW

DiaRest_RW <- t(DiaRes_RW)
DiaRest_RW

#Graphical Tests

par(mfrow=c(2,3))

# BoxPlot for all variables
# loop over column *names* instead of actual columns
sapply(names(Diamond_RW), function(cn_RW){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Diamond_RW[[cn_RW]]))
    # use the `main` param to put column name as plot title
    qqnorm(Diamond_RW[[cn_RW]], main=cn_RW)
  qqline(Diamond_RW[[cn_RW]])
})

par(mfrow=c(1,1))

#########################################
## Checking Correlations               ##
#########################################

corrgram(Diamond_RW, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Diamond Price Stats")

res_RW <- cor(Diamond_RW, method="spearman")
round(res_RW, 2)

#########################################
## Creating Baseline Model             ##
#########################################

Dialm_RW = lm(Price ~ Carat + Clarity + Color + Cut + Year + SourceAlrosa +
              SourceDeBeers + SourceDebswana,
            data=Diamond_RW, na.action=na.omit)
Dialm_RW
summary(Dialm_RW)

#########################################
## Creating Backward Selection Model   ##
#########################################

BckDialm_RW = step(Dialm_RW, direction="backward", details=TRUE)

BckDialm_RW
summary(BckDialm_RW)

#########################################
## Creating Forward  Selection Model   ##
#########################################

minmodel_RW <- lm(Price ~ 1, data=Diamond_RW, na.action=na.omit)

FwdDialm_RW = step(minmodel_RW, direction="forward", scope =(
  ~ Carat + Clarity + Color + Cut + Year + SourceAlrosa + SourceDeBeers + 
  SourceDebswana), details=TRUE)

FwdDialm_RW
summary(FwdDialm_RW)

#########################################
## Creating Criteria Selection Model   ##
#########################################

Dialm_RW = lm(Price ~ Carat + Clarity + Color + Cut + Year + SourceAlrosa +
                SourceDeBeers + SourceDebswana,
              data=Diamond_RW, na.action=na.omit)
stpdialm_RW <- step(Dialm_RW)
stpdialm_RW
summary(stpdialm_RW)

#########################################
## Evaluating the Models               ##
#########################################


###########################################
## Creating Model and Residual vectors    #
###########################################

DiaFit_RW <- predict(Dialm_RW)
DiaRes_RW <- residuals(Dialm_RW)

BckDiaFit_RW <- predict(BckDialm_RW)
BckDiaRes_RW <- residuals(BckDialm_RW)

FwdDiaFit_RW <- predict(FwdDialm_RW)
FwdDiaRes_RW <- residuals(FwdDialm_RW)

StpDiaFit_RW <- predict(stpdialm_RW)
StpDiaRes_RW <- residuals(stpdialm_RW)


#Numerically

shapiro.test(DiaRes_RW)
shapiro.test(BckDiaRes_RW)
shapiro.test(FwdDiaRes_RW)
shapiro.test(StpDiaRes_RW)

#Graphically

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(Dialm_RW)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section


par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(BckDialm_RW)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section


par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(FwdDialm_RW)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(stpdialm_RW)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section


     