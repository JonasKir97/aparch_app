# This file includes all instructions to run the app
base::source("https://raw.githubusercontent.com/JonasKir97/aparch_app/master/packageManager.R", encoding = "UTF-8")

#Check and install missing packages
apapp.pm.installMissingPackages(rPackageRepository = "https://cran.rstudio.com/")

#Load all required packages
apapp.pm.loadRequiredPackages()

#Source all required files from github
apapp.pm.sourceNeededFilesFromRepo()

aCOGARCH_SimulationApp()