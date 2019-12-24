##################################################
###  02-Logistic Regression                     ##
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
setwd("C:/02")
options(scipen=9)

##################################################
### Install Libraries                           ##
##################################################

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

##################################################
### Read data and do preliminary data checks    ##
##################################################

# Read "comma separated value" files (".csv")
# Autism data set
AutismAll_RW <- read.csv("AutismClass.csv", header = TRUE, sep = ",")


head(AutismAll_RW,5)  #Print a Few Observations to Verify

#Rename for easier interpretation

names(AutismAll_RW) <- c("Index_RW", "A01_RW", "A02_RW", "A03_RW", "A04_RW", "A05_RW", "A06_RW", "A07_RW",
                      "A08_RW", "A09_RW", "A010_RW", "Age_RW", "Gender_RW", "Jaundic_RW", "Nationality_RW",
                      "Rel_RW", "Autism_RW")

str(AutismAll_RW)

#Convert Autism to binary
AutismAll_RW$AutismNew_RW <- as.numeric(AutismAll_RW$Autism_RW)
AutismAll_RW$AutismNew_RW <- AutismAll_RW$AutismNew_RW-1

#Convert Nationality to Dummy variables
Nat_Dummies_RW <- model.matrix(~Nationality_RW -1, data=AutismAll_RW)
head(Nat_Dummies_RW,5)

#Combine the Datasets again
AutismAll_RW <- cbind(AutismAll_RW, Nat_Dummies_RW)

#Convert Rel to Dummy variables
Rel_Dummies_RW <- model.matrix(~Rel_RW -1, data=AutismAll_RW)
head(Rel_Dummies_RW,5)

#Combine the Datasets again
AutismAll_RW <- cbind(AutismAll_RW, Rel_Dummies_RW)


#Drop unneeded column
str(AutismAll_RW)
AutismAll_RW <- AutismAll_RW[-c(15,16,17)]   

#Drop the unique identifier
AutismAll_RW<-AutismAll_RW[-c(1)]
head(AutismAll_RW,5) 

ls(AutismAll_RW)      #Lists all objects for Autism data set
names(AutismAll_RW)   #List the variables for Autism data set
str(AutismAll_RW)     #List structure of Autism data set

#Rename the columns
names(AutismAll_RW) <- c("A01_RW", "A02_RW", "A03_RW", "A04_RW", "A05_RW", "A06_RW", "A07_RW",
                         "A08_RW", "A09_RW", "A010_RW", "Age_RW", "Gender_RW", "Jaundic_RW", 
                         "Autism_RW","NatAfr_RW","NatAsi_RW","NatEur_RW", 
                         "NatLatA_RW", "NatMidE_RW","NatNorA_RW",
                         "RelHCP_RW", "RelOth_RW", "RelPar_RW","RelRela_RW","RelSel_RW")

str(AutismAll_RW)
###################################################
## Univariate Descriptive Analysis               ##
###################################################

summary(AutismAll_RW)
stat.desc(AutismAll_RW)

par(mfrow=c(3,3))    #Fit more graphs in!

# Histogram for all variables
# loop over column *names* instead of actual columns
sapply(names(AutismAll_RW), function(cname_RW){
  # (make sure we only plot the numeric columns)
  if (is.numeric(AutismAll_RW[[cname_RW]]))
    # use the `main` param to put column name as plot title
    print(hist(AutismAll_RW[[cname_RW]], main=cname_RW))
})

par(mfrow=c(1,1))

###################################################
## Find Outliers                                 ##
###################################################

par(mfrow=c(3,3))

# BoxPlot for all variables
# loop over column *names* instead of actual columns
sapply(names(AutismAll_RW), function(cname_RW){
  # (make sure we only plot the numeric columns)
  if (is.numeric(AutismAll_RW[[cname_RW]]))
    # use the `main` param to put column name as plot title
    print(boxplot(AutismAll_RW[[cname_RW]], main=cname_RW))
})

par(mfrow=c(1,1))

quantile(AutismAll_RW$Age_RW,.99)

#### Based on Output, adjust maximum values

#Adjust Age
AutismAll_RW$Agen_RW <- with(AutismAll_RW, ifelse(Age_RW>56, 57,Age_RW))

##Check Again

par(mfrow=c(3,3))

# BoxPlot for all variables
# loop over column *names* instead of actual columns
sapply(names(AutismAll_RW), function(cname_RW){
  # (make sure we only plot the numeric columns)
  if (is.numeric(AutismAll_RW[[cname_RW]]))
    # use the `main` param to put column name as plot title
    print(boxplot(AutismAll_RW[[cname_RW]], main=cname_RW))
})

par(mfrow=c(1,1))

par(mfrow=c(3,3))    #Fit more graphs in!

# Histogram for all variables
# loop over column *names* instead of actual columns
sapply(names(AutismAll_RW), function(cname_RW){
  # (make sure we only plot the numeric columns)
  if (is.numeric(AutismAll_RW[[cname_RW]]))
    # use the `main` param to put column name as plot title
    print(hist(AutismAll_RW[[cname_RW]], main=cname_RW))
})

par(mfrow=c(1,1))

###################################################
## Comparing Correlation of Predictors           ##
## All                                           ##
###################################################

str(AutismAll_RW)
A_RW<- AutismAll_RW
str(A_RW)

corrgram(A_RW, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Autism Results")

res_RW <- cor(A_RW, method="spearman")
res_RW<-round(res_RW, 2)
res_RW

str(AutismAll_RW)
A_RW<- AutismAll_RW
str(A_RW)

#Drop the column of 15,21
AutismAll_RW<-AutismAll_RW[-c(15,21)]
str(AutismAll_RW)

#check again
A_RW<-AutismAll_RW
corrgram(A_RW, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Autism Results")

res_RW <- cor(A_RW, method="spearman")
res_RW<-round(res_RW, 2)
res_RW

###################################################
## Comparing Outcome with Predictors             ##
## Categorical Data                              ##
###################################################

cross_RW <- table(AutismAll_RW$Autism_RW, AutismAll_RW$A05_RW)

barplot(prop.table(cross_RW,2), xlab='Class',ylab='Frequency',main="Autism by A05",
        col=c("darkblue","darkred")
        ,legend=rownames(cross_RW), args.legend = list(x = "topleft"))

table(AutismAll_RW$A05_RW,AutismAll_RW$Autism_RW)   #Contingency Table
prop.table(table(AutismAll_RW$A05_RW, AutismAll_RW$Autism_RW), margin=1)*100  #Contingency Table Pct.
summary(table(AutismAll_RW$A05_RW,AutismAll_RW$Autism_RW))   #Chi-Sq
chisq.test(AutismAll_RW$A05_RW,AutismAll_RW$Autism_RW)       #Chi-Sq - specific



cross_RW <- table(AutismAll_RW$Autism_RW, AutismAll_RW$Gender_RW)

barplot(prop.table(cross_RW,2), xlab='Class',ylab='Frequency',main="Autism by Gender",
        col=c("darkblue","darkred")
        ,legend=rownames(cross_RW), args.legend = list(x = "topleft"))

table(AutismAll_RW$Gender_RW,AutismAll_RW$Autism_RW)   #Contingency Table
prop.table(table(AutismAll_RW$Gender_RW, AutismAll_RW$Autism_RW), margin=1)*100  #Contingency Table Pct.
summary(table(AutismAll_RW$Gender_RW,AutismAll_RW$Autism_RW))   #Chi-Sq
chisq.test(AutismAll_RW$Gender_RW,AutismAll_RW$Autism_RW)       #Chi-Sq - specific

#########################################
## Creating Baseline Model             ##
#########################################
str(AutismAll_RW)
AutGlm_RW = glm(Autism_RW ~ A01_RW + A02_RW + A03_RW + A04_RW + A05_RW + 
                   A06_RW + A07_RW + A08_RW + A09_RW + A010_RW + Agen_RW +
                   Gender_RW + Jaundic_RW + NatAsi_RW + NatEur_RW + NatLatA_RW +
                   NatMidE_RW + NatNorA_RW + RelOth_RW + RelPar_RW + RelRela_RW+
                   RelSel_RW, family="binomial", data=AutismAll_RW, na.action=na.omit)
AutGlm_RW
summary(AutGlm_RW)

#########################################
## Creating Stepwise Selection Model   ##
#########################################

AutGlm_RW = glm(Autism_RW ~ A01_RW + A02_RW + A03_RW + A04_RW + A05_RW + 
                   A06_RW + A07_RW + A08_RW + A09_RW + A010_RW + Agen_RW +
                   Gender_RW + Jaundic_RW + NatAsi_RW + NatEur_RW + NatLatA_RW +
                   NatMidE_RW + NatNorA_RW + RelOth_RW + RelPar_RW + RelRela_RW+
                   RelSel_RW, family="binomial", data=AutismAll_RW, na.action=na.omit)

StpAutGlm_RW <- step(AutGlm_RW)
StpAutGlm_RW
summary(StpAutGlm_RW)

#########################################
## Creating Adding Model 1             ##
#########################################

AutGlm_RW = glm(Autism_RW ~ A01_RW + A02_RW + A03_RW + A05_RW + 
                  A07_RW + A08_RW + 
                  Gender_RW + NatEur_RW +
                  NatMidE_RW + NatNorA_RW + RelRela_RW, 
                  family="binomial", data=AutismAll_RW, na.action=na.omit)

Stp1AutGlm_RW <- step(AutGlm_RW)
Stp1AutGlm_RW
summary(Stp1AutGlm_RW)


#########################################
## Creating Adding Model 2             ##
#########################################

AutGlm_RW = glm(Autism_RW ~ A01_RW + A02_RW + A03_RW + A05_RW + 
                  A07_RW + A08_RW + 
                  Gender_RW + NatEur_RW + 
                  NatNorA_RW + RelRela_RW, family="binomial", data=AutismAll_RW, na.action=na.omit)

Stp2AutGlm_RW <- step(AutGlm_RW)
Stp2AutGlm_RW
summary(Stp2AutGlm_RW)


#########################################
## Evaluating Logistic Model 1         ##
#########################################

### Confusion Matrix  ####
pred_RW <- predict(Stp1AutGlm_RW, AutismAll_RW)
#str(pred_RW)
head(pred_RW)
pred_y_RW <- as.numeric(pred_RW > -.5)
head(pred_y_RW)
true_y_RW <- as.numeric(AutismAll_RW$Autism_RW==1)
true_pos_RW <- (true_y_RW==1) & (pred_y_RW==1)
true_neg_RW <- (true_y_RW==0) & (pred_y_RW==0)
false_pos_RW <- (true_y_RW==0) & (pred_y_RW==1)
false_neg_RW <- (true_y_RW==1) & (pred_y_RW==0)

conf_mat_RW <- matrix(c(sum(true_pos_RW), sum(false_pos_RW),
                     sum(false_neg_RW), sum(true_neg_RW)),2,2)
colnames(conf_mat_RW) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat_RW) <- c('Y = 1', 'Y = 0')
conf_mat_RW

### Precision, Recall, Specificity ###

Stp1AutGlm_RW$Precis_RW <- conf_mat_RW[1,1]/sum(conf_mat_RW[,1])
Stp1AutGlm_RW$Recall_RW <- conf_mat_RW[1,1]/sum(conf_mat_RW[1,])
Stp1AutGlm_RW$Specif_RW <- conf_mat_RW[2,2]/sum(conf_mat_RW[2,])
Stp1AutGlm_RW$Accurary_RW <- (conf_mat_RW[1,1]+conf_mat_RW[2,2])/(sum(conf_mat_RW[,1])+sum(conf_mat_RW[,2]))

Stp1AutGlm_RW$Accurary_RW
Stp1AutGlm_RW$Specif_RW
Stp1AutGlm_RW$Recall_RW
Stp1AutGlm_RW$Precis_RW

### ROC Curve ###

idx_RW <- order(-pred_RW)
recall_RW <- cumsum(true_y_RW[idx_RW]==1)/sum(true_y_RW==1)
specificity_RW <- (sum(true_y_RW==0) - cumsum(true_y_RW[idx_RW]==0))/sum(true_y_RW==0)
roc_df_RW <- data.frame(recall_RW = recall_RW, specificity_RW = specificity_RW)
ggplot(roc_df_RW, aes(x=specificity_RW, y=recall_RW)) +
  geom_line(color='red') + 
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x), 
            linetype='dashed', color='blue')

### AUC ###

AUC_RW <- sum(roc_df_RW$recall_RW[-1] * diff(1-roc_df_RW$specificity_RW))
AUC_RW

PredTst_RW <- predict(Stp1AutGlm_RW, AutismAll_RW, type="response")

head(pred_RW)
pred_RW
PredTst_RW <- predict(Stp1AutGlm_RW, AutismAll_RW, type="response")
PredTst_RW
summary(PredTst_RW)

#########################################
## Evaluating Logistic Model 2         ##
#########################################

### Confusion Matrix  ####
pred_RW <- predict(Stp2AutGlm_RW, AutismAll_RW)
pred_y_RW <- as.numeric(pred_RW > -.5)
true_y_RW <- as.numeric(AutismAll_RW$Autism_RW==1)
true_pos_RW <- (true_y_RW==1) & (pred_y_RW==1)
true_neg_RW <- (true_y_RW==0) & (pred_y_RW==0)
false_pos_RW <- (true_y_RW==0) & (pred_y_RW==1)
false_neg_RW <- (true_y_RW==1) & (pred_y_RW==0)

conf_mat_RW <- matrix(c(sum(true_pos_RW), sum(false_pos_RW),
                        sum(false_neg_RW), sum(true_neg_RW)),2,2)
colnames(conf_mat_RW) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat_RW) <- c('Y = 1', 'Y = 0')
conf_mat_RW

### Precision, Recall, Specificity ###

Stp2AutGlm_RW$Precis_RW <- conf_mat_RW[1,1]/sum(conf_mat_RW[,1])
Stp2AutGlm_RW$Recall_RW <- conf_mat_RW[1,1]/sum(conf_mat_RW[1,])
Stp2AutGlm_RW$Specif_RW <- conf_mat_RW[2,2]/sum(conf_mat_RW[2,])
Stp2AutGlm_RW$Accurary_RW <- (conf_mat_RW[1,1]+conf_mat_RW[2,2])/(sum(conf_mat_RW[,1])+sum(conf_mat_RW[,2]))

Stp2AutGlm_RW$Accurary_RW
Stp2AutGlm_RW$Specif_RW
Stp2AutGlm_RW$Recall_RW
Stp2AutGlm_RW$Precis_RW

### ROC Curve ###

idx_RW <- order(-pred_RW)
recall_RW <- cumsum(true_y_RW[idx_RW]==1)/sum(true_y_RW==1)
specificity_RW <- (sum(true_y_RW==0) - cumsum(true_y_RW[idx_RW]==0))/sum(true_y_RW==0)
roc_df_RW <- data.frame(recall_RW = recall_RW, specificity_RW = specificity_RW)
ggplot(roc_df_RW, aes(x=specificity_RW, y=recall_RW)) +
  geom_line(color='red') + 
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x), 
            linetype='dashed', color='blue')

### AUC ###

AUC_RW <- sum(roc_df_RW$recall_RW[-1] * diff(1-roc_df_RW$specificity_RW))
AUC_RW

PredTst_RW <- predict(Stp2AutGlm_RW, AutismAll_RW, type="response")

head(pred_RW)
pred_RW
PredTst_RW <- predict(Stp2AutGlm_RW, AutismAll_RW, type="response")
PredTst_RW
summary(PredTst_RW)

