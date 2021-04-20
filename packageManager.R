requiredPackages <- c("shiny","shinythemes","shinyjs","ggplot2","Rcpp")

installMissingPackages <- function(rPackageRepository = "https://cran.rstudio.com/"){
  availablePackages <- unname(utils::installed.packages()[,1])
  
  needToInstall <- requiredPackages[!(requiredPackages %in% availablePackages)]
  
  if(!length(needToInstall)) {
    print("Alle nötigen Packages sind bereits installiert.")
    return()
  }
  
  print("Folgende Packages sind noch nicht vorhanden und werden installiert:")
  print(paste0(needToInstall,collapse=","))
  
  utils::install.packages(pkgs = needToInstall, repos = rPackageRepository)
  
  print("Alle nötigen Packages wurden installiert.")
}

loadRequiredPackages <- function() {
  for(p in requiredPackages) library(p, character.only = TRUE)
}