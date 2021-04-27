apapp.pm.getRequiredPackages <- function(useHighcharts, useCpp)  {
  requiredPackages <- c("shiny","shinythemes","shinyjs","shinycssloaders","ggplot2")
  if(useCpp) requiredPackages <- c(requiredPackages,"Rcpp")
  if(useHighcharts) requiredPackages <- c(requiredPackages,"highcharter","xts")
  return(requiredPackages)
}

apapp.pm.installMissingPackages <- function(rPackageRepository = "https://cran.rstudio.com/",
                                            requiredPackages){
  availablePackages <- unname(utils::installed.packages()[,1])
  
  needToInstall <- requiredPackages[!(requiredPackages %in% availablePackages)]
  
  if(!length(needToInstall)) {
    print("Alle nötigen Packages sind bereits installiert.")
    return()
  }
  
  print("Folgende Packages sind noch nicht vorhanden und werden installiert:")
  print(paste0(needToInstall,collapse=","))
  Sys.sleep(1)
  utils::install.packages(pkgs = needToInstall, repos = rPackageRepository)
  
  print("Alle nötigen Packages wurden installiert.")
}

apapp.pm.loadRequiredPackages <- function(requiredPackages) {
  for(p in requiredPackages) library(p, character.only = TRUE)
}

apapp.pm.sourceNeededFiles <- function(gitDirectoryPath, sourceCppToo = TRUE) {
  allFiles <- list.files(gitDirectoryPath, full.names = TRUE)
  
  rFiles <- allFiles[grepl("\\.R$",allFiles)]
  rFiles <- rFiles[!grepl("packageManager",rFiles)]
  
  print("Die folgenden RDateien werden gesourct:")
  print(paste0(rFiles, collapse=", "))
  
  for(rFile in rFiles) {
    print(paste0("Source R-Datei ",rFile,"."))
    source(file = rFile, encoding = "UTF-8")
  }
  
  if(sourceCppToo) {
    cppFiles <- allFiles[grepl("\\.cpp$",allFiles)]
    print("Die folgenden C++ Dateien werden gesourct:")
    print(paste0(cppFiles, collapse=", "))
    for(cppFile in cppFiles) {
      print(paste0("Source cpp-Datei ",cppFile,"."))
      Rcpp::sourceCpp(file = cppFile)
    }
  }
  
  print("Alle nötigen Dateien gesourct. Die App kann nun durch den Aufruf 'aCOGARCH_SimulationApp()' gestartet werden.")
}


apapp.pm.sourceNeededFilesFromRepo <- function(useCpp) {
  rFiles <- c("https://raw.githubusercontent.com/JonasKir97/aparch_app/master/acogarchSimulation.R",
              "https://raw.githubusercontent.com/JonasKir97/aparch_app/master/acogarchSimulationAppHelpers.R",
              "https://raw.githubusercontent.com/JonasKir97/aparch_app/master/guiHelpers.R",
              "https://raw.githubusercontent.com/JonasKir97/aparch_app/master/nasdaqDataReader.R",
              "https://raw.githubusercontent.com/JonasKir97/aparch_app/master/plotHelpers.R",
              "https://raw.githubusercontent.com/JonasKir97/aparch_app/master/firstJumpApproximation.R")
  
  for(rFile in rFiles) {
    print(paste0("Source R-Datei ",rFile,"."))
    base::source(file = rFile, encoding = "UTF-8")
  }
  
  if(useCpp) {
    cppFiles <- c("https://raw.githubusercontent.com/JonasKir97/aparch_app/master/simulateDiscreteAPARCH11inCPP.cpp")
    
    sourceCppFromGithub <- function(url) {
      tf <- tempfile(fileext = ".cpp") 
      utils::download.file(url, tf, quiet=TRUE) 
      Rcpp::sourceCpp(file = tf)
      file.remove(tf)
    }
    
    for(cFile in cppFiles) {
      print(paste0("Source cpp-Datei ",cFile,"."))
      sourceCppFromGithub(cFile)
    }
  }

  print("Sourcing done.")
}
