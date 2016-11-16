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



#scatter plot amongst the seemingly similar variables(continuous)

churnScatter1 <- churn[, c("Day.Mins", "Day.Calls", "Day.Charge")]
colnames(churnScatter1) <-
  c("Day minutes", "Day Calls", "Day Charge")
plot(churnScatter1)

#Correlation matrix (description in the document)
#On the basis of corelation we eliminate 4 variables, since there were a linear
#function of other 4 variables
cor(churnScatter1[sapply(churnScatter1, is.numeric)])

#If the p-value is greater than .1, it will not be predictive of churn
t.test(churn$Day.Calls ~ churn$Churn)
#Eliminate Day Calls Calls
t.test(churn$Day.Mins ~ churn$Churn)
#Retain Day Minutes


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

#scatter plot amongst the seemingly similar variables(continuous)

churnScatter2 <- churn[, c("Eve.Mins", "Eve.Calls", "Eve.Charge")]
colnames(churnScatter2) <-
  c("Eve minutes", "Eve Calls", "Eve Charge")
plot(churnScatter2)

#Correlation matrix (description in the document)
#On the basis of corelation we eliminate 4 variables, since there were a linear
#function of other 4 variables
cor(churnScatter2[sapply(churnScatter2, is.numeric)])
#If the p-value is greater than .1, it will not be predictive of churn
t.test(churn$Eve.Calls ~ churn$Churn)
#Eliminate Eve Calls
t.test(churn$Eve.Mins ~ churn$Churn)
#retain Eve minutes

#Day min Vs Eve min
#conclusion: Higher the day min and evening min, more the churn
qplot(churn$Eve.Mins,churn$Day.Mins,
      data = churn,
      colour = churn$Churn., xlab = "Evening Minutes",
      ylab= "Day Minutes")


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

#If the p-value is greater than .1, it will not be predictive of churn
t.test(churn$Night.Calls ~ churn$Churn)
#Eliminate Night Calls
t.test(churn$Night.Mins ~ churn$Churn)
#retain Night minutes

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
#If the p-value is greater than .1, it will not be predictive of churn
t.test(churn$Intl.Calls ~ churn$Churn)
#Retain International Calls
t.test(churn$CustServ.Calls ~ churn$Churn)
#retain Customer Service Calls

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

churnScatter3 <-
  churn[, c("Intl.Mins", "Intl.Calls", "Intl.Charge")]
colnames(churnScatter3) <-
  c("Intl minutes", "Intl Calls", "Intl Charge")
plot(churnScatter3)

#Correlation matrix (description in the document)
#On the basis of corelation we eliminate 4 variables, since there were a linear
#function of other 4 variables
cor(churnScatter3[sapply(churnScatter2, is.numeric)])

#Intl Plan

#table for counts of Churn and International Plan
countsIntlPlan <- table(churn$Churn,
                        churn$Int.l.Plan,
                        dnn = c("Churn", "International Plan"))


#Pie chart wich shows that people who have international plan may churn
slices <- c(countsIntlPlan[1, 2] , countsIntlPlan[2, 2])
lbls <- c("churn: False", "churn: True")
pct <- round(slices / sum(slices) * 100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls, "%", sep = "") # ad % to labels
pie(slices,
    labels = lbls,
    col = rainbow(length(lbls)),
    main = "People Having International Plan")



#Overlayed bar chart
barplot(
  countsIntlPlan,
  legend = rownames(countsIntlPlan),
  col = c("blue", "red"),
  ylim = c(0, 3300),
  ylab = "Count",
  xlab = "International Plan",
  main = "Comparison Bar Chart:
  Churn Proportions by International Plan"
)
box(which = "plot",
    lty = "solid",
    col = "black")



#Clustered Bar Chart of Churn and Intl Plan with legend
barplot(
  t(countsIntlPlan),
  col = c("blue", "green"),
  ylim = c(0, 3300),
  ylab = "Counts",
  xlab = "Churn",
  main = "International Plan Count by Churn",
  beside = TRUE
)
legend(
  "topright",
  c(rownames(countsIntlPlan)),
  col = c("blue", "green"),
  pch = 15,
  title = "Intl Plan"
)
box(which = "plot",
    lty = "solid",
    col = "black")

#Vmail Plan
#weak evidence, but still vmail plan may be predictive
#'cause we can see int row.margin[2,1] and row.margin[2,2]
#that the people who dont have the vmail plan and will churn % = 84
#have vmail and will churn % = 16

countsVmailPlan <- table(churn$Churn, churn$VMail.Plan,
                         dnn = c("Churn", "Vmail Plan"))


row.margin <- round(prop.table(countsVmailPlan, margin = 1), 4)*100
row.margin

#Vmail message's histogram gives us a spike
#For the analysis we say that If Voice Mail Messages > 0 then VoiceMailMessages_Flag = 1;
#otherwise VoiceMailMessages_Flag = 0
#it reveals that it is similar to the vmal plan, hence we can eliminate vmail message

churn$flag[churn$VMail.Message>0] <- 1
churn$flag[churn$VMail.Message<=0] <- 0
table(churn$flag,churn$Churn)


#---------------------Graphical Evidence to retain ------------------------------
#---------------------above variables(Customer Service Call)---------------------
hist(
  churn$CustServ.Calls,
  border = "blue",
  col = "green",
  main = "Histogram for Customer Service Calls",
  xlab = "Customer Service Calls"
)

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


#------------------Multivariate relationships-------------------------------------

#cust serv calls vs day calls
#Conclusion: hiher the
qplot(churn$Day.Mins,
      churn$CustServ.Calls,
      data = churn,
      colour = churn$Churn.)

#Conclusion: Customer Service Calls is predictive of churn
