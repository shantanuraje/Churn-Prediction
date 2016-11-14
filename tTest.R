#If the p-value is greater than .1, it will not be predictive of churn

t.test(churn$Intl.Calls ~ churn$Churn)
#Retain International Calls

t.test(churn$Day.Calls ~ churn$Churn)
#Eliminate Day Calls Calls

t.test(churn$Night.Calls ~ churn$Churn)
#Eliminate Night Calls

t.test(churn$Eve.Calls ~ churn$Churn)
#Eliminate Eve Calls

t.test(churn$Day.Mins ~ churn$Churn)
#Retain Day Minutes

t.test(churn$Eve.Mins ~ churn$Churn)
#retain Eve minutes

t.test(churn$Night.Mins ~ churn$Churn)
#retain Night minutes

t.test(churn$CustServ.Calls ~ churn$Churn)
#retain Customer Service Calls

#conclusion: Retain Intl Calls, Eve minutes, Nigh, Eve, Day Minutes
#, Customer Service Calls

