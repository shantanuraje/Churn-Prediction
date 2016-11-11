#-------------------------Package Requirements--------------------------------------------
installedPackages = installed.packages()
installedPackages = installedPackages[,1]
requiredPackages = as.matrix(c('ggplot2','randomForest','RWeka'))
installAndLoad<-function(package){
  searchResult<- grep(paste(package,"$",sep = ""),installedPackages)
#  print(searchResult)
  print(length(searchResult))
  if(length(searchResult) == 1){
    print (paste("Search result",package,"Installed"))
    library(package,character.only = TRUE)
  }else {
    print (paste(package,"not installed"))
    print("Downloading and Installing the package")
    install.packages(package)
    library(package,character.only = TRUE)
  }
}
apply(requiredPackages, 1, installAndLoad)

