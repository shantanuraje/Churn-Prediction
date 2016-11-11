library(dplyr)
library(ggplot2)
library(GGally)
churnData = read.csv("./churn_tel.csv")

#display the Structure of an Arbitrary R Object
str(churnData)

# Observe first few rows of dataset
print(head(churnData))

# Observe first few rows of dataset
print(tail(churnData))

#check unique states
states <- sort(unique(churnData$State))
print(states)
print(sort(state.abb)) #us states database in R
#conclusion - extra state DC present

#explore account length i.e how long the account has been active
accLenDist <- hist(churnData$Account.Length,xlab = "Length of Account", ylab = "Number of accounts",main = "Account length distribution")
#scatterplots for days attributes
#days <- ggplot(churnData,mapping = aes(x = c(Day.Mins,Day.Calls,Day.Charge),y = c(Day.Mins,Day.Calls,Day.Charge)))
#days <- days + geom_point(size=1)
days <- ggpairs(churnData[,8:10], colour="gray20")
print(days)
evenings <- ggpairs(churnData[,11:13], colour="gray20")
print(evenings)
nights <- ggpairs(churnData[,14:16], colour="gray20")
print(nights)
international <- ggpairs(churnData[,17:19], colour="gray20")
print(international)
