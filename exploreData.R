library(lattice)
dataset = read.csv("./churn_tel.csv")

#Data exploration

# Scatter plots for days
#plot(dataset$Day.Mins,dataset$Day.Calls) 
#plot(dataset$Day.Calls,dataset$Day.Charge) 
#plot(dataset$Day.Mins,dataset$Day.Charge)

# Scatter plots for evenings
#plot(dataset$Eve.Mins,dataset$Eve.Calls) 
#plot(dataset$Eve.Calls,dataset$Eve.Charge) 
#plot(dataset$Eve.Mins,dataset$Eve.Charge)

# Scatter plots for nights
#plot(dataset$Night.Mins,dataset$Night.Calls) 
#plot(dataset$Night.Calls,dataset$Night.Charge) 
#plot(dataset$Night.Mins,dataset$Night.Charge)

# Scatter plots for international service
#plot(dataset$Intl.Mins ,dataset$Intl.Calls) 
#plot(dataset$Intl.Calls,dataset$Intl.Charge) 
#plot(dataset$Intl.Mins,dataset$Intl.Charge)

#scatterplot matrix for days
splom(dataset[c(8,9,10)])
#scatterplot matrix for evenings
splom(dataset[c(11,12,13)])
#scatterplot matrix for nights
splom(dataset[c(14,15,16)])
#scatterplot matrix for international service
splom(dataset[c(17,18,19)])
# perform regression analysis on above categories, remove perfectly correlated linear (redundant variables)

#plot international plan vs churn
plot(dataset$Int.l.Plan,dataset$Churn.)

#plot voicemail plan vs churn
plot(dataset$VMail.Plan,dataset$Churn.)

#plot international plan vs churn
#plot(dataset$CustServ.Calls,dataset$Churn.)
