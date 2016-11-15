#Histograms (run them one by one)

hist(
  churn$Day.Mins,
  border = "blue",
  col = "green",
  main = "Histogram for Day Minutes",
  xlab = "Day minutes"
)

hist(
  churn$Day.Calls,
  border = "blue",
  col = "green",
  main = "Histogram for Day Calls",
  xlab = "Day Calls"
)

hist(
  churn$Day.Charge,
  border = "blue",
  col = "green",
  main = "Histogram for Day Charge",
  xlab = "Day Charge"
)

# Box plots for Day Mins, Charge, Calls vs Churn
DayMinsChurn=lm(churn$Day.Mins~churn$Churn)
summary(DayMinsChurn)
plot(churn$Day.Mins~churn$Churn)

DayChargeChurn=lm(churn$Day.Charge~churn$Churn)
summary(DayChargeChurn)
plot(churn$Day.Charge~churn$Churn)

DayCallsChurn=lm(churn$Day.Calls~churn$Churn)
summary(DayCallsChurn)
plot(churn$Day.Calls~churn$Churn)

# Box plots for Evening Mins, Charge, Calls vs Churn
EveMinsChurn=lm(churn$Eve.Mins~churn$Churn)
summary(EveMinsChurn)
plot(churn$Eve.Mins~churn$Churn)

EveChargeChurn=lm(churn$Eve.Charge~churn$Churn)
summary(EveChargeChurn)
plot(churn$Eve.Charge~churn$Churn)

EvenCallsChurn=lm(churn$Eve.Calls~churn$Churn)
summary(EvenCallsChurn)
plot(churn$Eve.Calls~churn$Churn)

# Box plots for Night Mins, Charge, Calls vs Churn
NightMinsChurn=lm(churn$Night.Mins~churn$Churn)
summary(NightMinsChurn)
plot(churn$Night.Mins~churn$Churn)

NightChargeChurn=lm(churn$Night.Charge~churn$Churn)
summary(NightChargeChurn)
plot(churn$Night.Charge~churn$Churn)

NightCallsChurn=lm(churn$Night.Calls~churn$Churn)
summary(NightCallsChurn)
plot(churn$Night.Calls~churn$Churn)

# Box plots for International Mins, Charge, Calls vs Churn
IntlMinsChurn=lm(churn$Intl.Mins~churn$Churn)
summary(IntlMinsChurn)
plot(churn$Intl.Mins~churn$Churn)

IntlChargeChurn=lm(churn$Intl.Charge~churn$Churn)
summary(IntlChargeChurn)
plot(churn$Intl.Charge~churn$Churn)

IntlCallsChurn=lm(churn$Intl.Calls~churn$Churn)
summary(IntlCallsChurn)
plot(churn$Intl.Calls~churn$Churn)


#scatter plot amongst the seemingly similar variables(continuous)

churnScatter1 <- churn[, c("Day.Mins", "Day.Calls", "Day.Charge")]
colnames(churnScatter1) <-
  c("Day minutes", "Day Calls", "Day Charge")
plot(churnScatter1)

#Correlation matrix (description in the document)
#On the basis of corelation we eliminate 4 variables, since there were a linear
#function of other 4 variables
cor(churnScatter1[sapply(churnScatter1, is.numeric)])

hist(
  churn$Eve.Mins,
  border = "blue",
  col = "green",
  main = "Histogram for Eve Minutes",
  xlab = "Eve minutes"
)

hist(
  churn$Eve.Calls,
  border = "blue",
  col = "green",
  main = "Histogram for Eve Calls",
  xlab = "Eve Calls"
)

hist(
  churn$Eve.Charge,
  border = "blue",
  col = "green",
  main = "Histogram for Eve Charge",
  xlab = "Eve Charge"
)

#scatter plot amongst the seemingly similar variables(continuous)

churnScatter2 <- churn[, c("Eve.Mins", "Eve.Calls", "Eve.Charge")]
colnames(churnScatter2) <-
  c("Eve minutes", "Eve Calls", "Eve Charge")
plot(churnScatter2)

#Correlation matrix (description in the document)
#On the basis of corelation we eliminate 4 variables, since there were a linear
#function of other 4 variables
cor(churnScatter2[sapply(churnScatter2, is.numeric)])

hist(
  churn$Night.Mins,
  border = "blue",
  col = "green",
  main = "Histogram for Night Minutes",
  xlab = "Night minutes"
)

hist(
  churn$Night.Calls,
  border = "blue",
  col = "green",
  main = "Histogram for Night Calls",
  xlab = "Night Calls"
)

hist(
  churn$Night.Charge,
  border = "blue",
  col = "green",
  main = "Histogram for Night Charge",
  xlab = "Night Charge"
)

#scatter plot amongst the seemingly similar variables(continuous)

churnScatter4 <-
  churn[, c("Night.Mins", "Night.Calls", "Night.Charge")]
colnames(churnScatter4) <-
  c("Night minutes", "Night Calls", "Night Charge")
plot(churnScatter4)


#Correlation matrix (description in the document)
#On the basis of corelation we eliminate 4 variables, since there were a linear
#function of other 4 variables
cor(churnScatter4[sapply(churnScatter2, is.numeric)])

hist(
  churn$Intl.Mins,
  border = "blue",
  col = "green",
  main = "Histogram for Night Minutes",
  xlab = "International minutes"
)

hist(
  churn$Intl.Calls,
  border = "blue",
  col = "green",
  main = "Histogram for Night Calls",
  xlab = "International Calls"
)

hist(
  churn$Intl.Charge,
  border = "blue",
  col = "green",
  main = "Histogram for Night Charge",
  xlab = "International Charge"
)

#scatter plot amongst the seemingly similar variables(continuous)

churnScatter3 <-
  churn[, c("Intl.Mins", "Intl.Calls", "Intl.Charge")]
colnames(churnScatter3) <-
  c("Intl minutes", "Intl Calls", "Intl Charge")
plot(churnScatter3)

#Correlation matrix (description in the document)
#On the basis of corelation we eliminate 4 variables, since there were a linear
#function of other 4 variables
cor(churnScatter3[sapply(churnScatter2, is.numeric)])

hist(
  churn$CustServ.Calls,
  border = "blue",
  col = "green",
  main = "Histogram for Customer Service Calls",
  xlab = "Customer Service Calls"
)


#---------------------Graphical Evidence to retain ------------------------------
#---------------------above variables(Customer Service Call)---------------------
#Customer Service Calls vs Churn

ggplot() +
  geom_bar(data = churn,
           aes(
             x = factor(churn$CustServ.Calls),
             fill = factor(churn$Churn)
           ),
           position = "fill") +
  scale_x_discrete("Customer Service Calls") +
  scale_y_continuous("Percent") +
  guides(fill = guide_legend(title = "Churn")) +
  scale_fill_manual(values = c("green", "red"))
#Conclusion: Customer Service Calls is predictive of churn


