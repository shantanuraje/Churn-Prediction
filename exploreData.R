dataset = read.csv("./churn_tel.csv")

#Data exploration
noOfRecords = nrow(dataset)
noOfFeatures = ncol(dataset);

#cat("Number of Records =",noOfRecords,"\n");
#cat("Number of Featues =",noOfFeatures,"\n");

#Print get dataset information
str(dataset)
datasetSummary = summary(dataset$State)
#print(datasetSummary)
#print(unique(dataset$State))
#for (i in dataset$State) {
#  print(i)
#}
# for(i in 1:ncol(dataset)) {
#   test = dataset[,i]
#   break()
  
  # do stuff with row
#}
#sort(state.abb) == sort(unique(dataset$State))
main = sort(state.abb)
test = sort(unique(dataset$State))
# print(typeof(main[1]))
# print(typeof(test[1]))
# print(int(main[1]))
print(test[1])
#table(factor(dataset$State)) #count number of records for each state
