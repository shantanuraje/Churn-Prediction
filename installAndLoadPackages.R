#-------------------------Package Requirements--------------------------------------------
installedPackages = installed.packages()
installedPackages = installedPackages[,1]
requiredPackages = as.matrix(c('ggplot2','randomForest','RWeka'))
installPackages<-function(package){
  searchResult<- grep(paste(package,"$",sep = ""),installedPackages)
  print(length(searchResult))
  if(length(searchResult) == 0){
    print (paste(package,"not installed"))
    print("Downloading and Installing the package")
    install.packages(package)
  }
}

loadPackages<-function(package){
  print (paste("Loading",package))
  require(package,character.only = TRUE)
}

apply(requiredPackages, 1, installPackages)
apply(requiredPackages, 1, loadPackages)

