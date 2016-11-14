
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
#------------------Multivariate relationships-------------------------------------

#cust serv calls vs day calls
#Conclusion: hiher the
qplot(churn$Day.Mins,
      churn$CustServ.Calls,
      data = churn,
      colour = Churn)


#Day min Vs Eve min
#conclusion: Higher the day min and evening min, more the churn
qplot(churn$Eve.Mins,churn$Day.Mins,
      data = churn,
      colour = Churn, xlab = "Evening Minutes",
      ylab= "Day Minutes")


