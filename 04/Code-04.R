##################################################
### 04-Time Series                              ## 
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
setwd("C:/04")

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


##############Section 1 Welland Rain##############

##################################################
### Load the data and Transformation            ##
##################################################

Rain_RW <- read.csv("Welland_Rain.csv", header=TRUE)
Rain_RW=Rain_RW$Precip
head(Rain_RW)

RainStudy_RW <- ts(Rain_RW, frequency = 12,start=c(1995,1))
head(RainStudy_RW)

##################################################
##Descriptive Data Analysis                     ##
##################################################
summary(RainStudy_RW)

stat.desc(RainStudy_RW)
RainStudy_RW

#boxplot(RainStudy_RW)

#Plot time series
plot.ts(RainStudy_RW, main="Total precipitation of Wellan",  ylim = c(10, 190)) 

#Decompose
decompTemp_RW <- decompose(RainStudy_RW, type="additive") 
decompTemp_RW
plot(decompTemp_RW)

#check stationary
adf.test(RainStudy_RW)

#Deseasonalize the precipitation information
RainStudySeasAdj_RW <- RainStudy_RW - decompTemp_RW$seasonal

plot.ts(RainStudySeasAdj_RW, main="Deseasonalized - Total precipitation of Wellan", ylim = c(10, 190))

########Section 2 Waterloo Precipitation#########

##################################################
### Load the data and Transformation            ##
##################################################
Precip_RW <- read.csv("Waterloo_Precip.csv", header=TRUE)
head(Precip_RW)
Precip_RW <- Precip_RW[c(-1)]
head(Precip_RW)

Precip_RW <- ts(Precip_RW, frequency = 1, start=c(1950))  
head(Precip_RW) 

##################################################
##Descriptive Data Analysis                     ##
##################################################
summary(RainStudy_RW)
stat.desc(Precip_RW)

#Plot time series
plot.ts(Precip_RW, main="Total precipitation of Waterloo")

#Simple Moving Average
PrecipSMA1_RW <- SMA(Precip_RW,n=3)
plot.ts(PrecipSMA1_RW)

PrecipSMA2_RW <- SMA(Precip_RW,n=5)
plot.ts(PrecipSMA2_RW)

PrecipSMA3_RW <- SMA(Precip_RW,n=10)
plot.ts(PrecipSMA3_RW)

#check stationary
adf.test(Precip_RW)

#Autocorrelations
acf(Precip_RW)   

#########Forcast###################
#Simple Moving Average
SMA_RW <- sma(Precip_RW)
SMA_RW


SMA_RW <- forecast(SMA_RW, h=5,level=0.75)
SMA_RW
plot(SMA_RW)

#Exponential Smoothing Forecast
ES_RW <- es(Precip_RW)
ES_RW

ES_RW <- forecast(Precip_RW, h=5,level=0.75)
ES_RW
plot(ES_RW)

