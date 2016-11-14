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


hist(
  churn$CustServ.Calls,
  border = "blue",
  col = "green",
  main = "Histogram for Customer Service Calls",
  xlab = "Customer Service Calls"
)



#scatter plot amongst the seemingly similar variables(continuous)

churnScatter1 <- churn[, c("Day.Mins", "Day.Calls", "Day.Charge")]
colnames(churnScatter1) <-
  c("Day minutes", "Day Calls", "Day Charge")
plot(churnScatter1)

churnScatter2 <- churn[, c("Eve.Mins", "Eve.Calls", "Eve.Charge")]
colnames(churnScatter2) <-
  c("Eve minutes", "Eve Calls", "Eve Charge")
plot(churnScatter2)


churnScatter3 <-
  churn[, c("Intl.Mins", "Intl.Calls", "Intl.Charge")]
colnames(churnScatter3) <-
  c("Intl minutes", "Intl Calls", "Intl Charge")
plot(churnScatter3)

churnScatter4 <-
  churn[, c("Night.Mins", "Night.Calls", "Night.Charge")]
colnames(churnScatter4) <-
  c("Night minutes", "Night Calls", "Night Charge")
plot(churnScatter4)

#Correlation matrix (description in the document)
#On the basis of corelation we eliminate 4 variables, since there were a linear
#function of other 4 variables


cor(churnScatter1[sapply(churnScatter1, is.numeric)])
cor(churnScatter2[sapply(churnScatter2, is.numeric)])
cor(churnScatter3[sapply(churnScatter2, is.numeric)])
cor(churnScatter4[sapply(churnScatter2, is.numeric)])
