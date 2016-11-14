#creates a new file decisionTree to test against the test data
#----------------------------------------------------------------------------------

#churn <- read.csv("churn_tel.csv")
churnTrain2 <- churn[800:3300, ]
churnTest2 <- churn[1:500, ]


decisionTree <- J48(`Churn.` ~ ., data = churnTrain2)
prediction_tree <- predict(decisionTree,  churnTest2)
count2 <- table(churnTest2$Churn, prediction_tree)
count2


#Accuracy of J48(using pie chart)


slices <-
  c(count2[1, 1] + count2[2, 2], count2[1, 2] + count2[2, 1])
lbls <- c("correct prediction", "incorrect prediction")
pct <- round(slices / sum(slices) * 100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls, "%", sep = "") # ad % to labels
pie(slices,
    labels = lbls,
    col = rainbow(length(lbls)),
    main = "J48 Decision Tree Accuracy")

