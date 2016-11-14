#--------------------------Telecommunication Churn------------------------------------
# Dataset Name: churn_tel.csv

#-------------------------Install and Load Packages--------------------------------------------
#source("installAndLoadPackages.R")
#-------------------------------------------------------------------------------------

#-------------------------Data Exploratory--------------------------------------------
# i) Description of the Data Set
#-------------------------------------------------------------------------------------
rm(list=ls())

# Check for the Directory of the R language
getwd()

#copy the churn_tel.csv to this location, view the files present in the directory
dir()

#import the file in the above directory, then read it here
churn <- read.csv("churn_tel.csv")

#Names of all the attributes in the data set
names(churn)

#summary of the dataset
summary(churn)

#Churn prediction based on  Minutes
ls=lm(churn$Day.Mins~churn$Churn)
summary(ls)
plot(churn$Day.Mins~churn$Churn)

ls1=lm(churn$Eve.Mins~churn$Churn)
summary(ls1)
plot(churn$Eve.Mins~churn$Churn)

ls2=lm(churn$Night.Mins~churn$Churn)
summary(ls2)
plot(churn$Night.Mins~churn$Churn)

ls3=lm(churn$Intl.Mins~churn$Churn)
summary(ls3)
plot(churn$Intl.Mins~churn$Churn)

#churn predictions based on Charges

ls4=lm(churn$Day.Charge~churn$Churn)
summary(ls4)
plot(churn$Day.Charge~churn$Churn)

ls5=lm(churn$Night.Charge~churn$Churn)
summary(ls5)
plot(churn$Night.Charge~churn$Churn)

ls6=lm(churn$Intl.Charge~churn$Churn)
summary(ls6)
plot(churn$Intl.Charge~churn$Churn)

ls7=lm(churn$Eve.Charge~churn$Churn)
summary(ls7)
plot(churn$Eve.Charge~churn$Churn)

#churn predictions based on calls

ls8=lm(churn$Day.Calls~churn$Churn)
summary(ls8)
plot(churn$Day.Calls~churn$Churn)

ls9=lm(churn$Night.Calls~churn$Churn)
summary(ls9)
plot(churn$Night.Calls~churn$Churn)

ls10=lm(churn$Eve.Calls~churn$Churn)
summary(ls10)
plot(churn$Eve.Calls~churn$Churn)

ls11=lm(churn$Intl.Calls~churn$Churn)
summary(ls11)
plot(churn$Intl.Calls~churn$Churn)

ls10=lm(churn$CustServ.Calls~churn$Churn)
summary(ls10)
plot(churn$CustServ.Calls~churn$Churn)



















