# i) Description of the Data Set
#-------------------------------------------------------------------------------------

# Check for the Directory of the R language
print(getwd())

#copy the churn_tel.csv to this location, view the files present in the directory
print(dir())

#import the file in the above directory, then read it here
churn <- read.csv("churn_tel.csv")

# Compactly Display the Structure of churn dataset
print(str(churn))

#Names of all the attributes in the data set
print(names(churn))

#summary of the dataset
print(summary(churn))

#Omit and row which has missing value(there are none), it returns 0 rows
print(churn[!complete.cases(churn),])