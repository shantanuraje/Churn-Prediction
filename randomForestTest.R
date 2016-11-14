#creates a new file firstforest to test against the test data
#----------------------------------------------------------------------------------

#churn <- read.csv("churn_tel.csv")
churnTrain <- churn[800:3300, ]
churnTest <- churn[1:500, ]


fit <-
  randomForest(
    as.factor(Churn.) ~ Int.l.Plan + VMail.Plan + CustServ.Calls + Day.Mins +
      Eve.Mins + VMail.Message + Night.Mins + Intl.Mins + Intl.Calls,
    data = churnTrain,
    importance = TRUE,
    ntree = 500,
    nodesize = 3
  )
Prediction <- predict(fit, churnTest)
submit <- data.frame(id = churnTest$Id, Churn = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)
fr <- read.csv("firstforest.csv")
count <- table(churnTest$Churn, fr$Churn)
count

#Accuracy of Random Forest(using pie chart)


slices <- c(count[1, 1] + count[2, 2], count[1, 2] + count[2, 1])
lbls <-  c("correct prediction", "incorrect prediction")
pct <- round(slices / sum(slices) * 100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls, "%", sep = "") # ad % to labels
pie(slices,
    labels = lbls,
    col = rainbow(length(lbls)),
    main = "Random Forest Accuracy")


