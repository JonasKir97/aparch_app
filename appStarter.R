# This file includes all instructions to run the app
base::source("https://raw.githubusercontent.com/JonasKir97/aparch_app/master/packageManager.R", encoding = "UTF-8")

# Settings for the app that can be specified by the user
appSettings <- list(
  useHighcharts = TRUE,  #use highcharts instead of ggplot
  useCpp = TRUE, #use Cpp in some steps instead of R-Code (much fatser computation times, given Rcpp is avaialble on the system)
  appPort = 2021 #port where the app is running on localhost
)

apapp.pm.requiredPackages <- apapp.pm.getRequiredPackages(useHighcharts = appSettings$useHighcharts, 
                                                          useCpp = appSettings$useCpp)

#Check and install missing packages
apapp.pm.installMissingPackages(rPackageRepository = "https://cran.rstudio.com/", 
                                requiredPackages = apapp.pm.requiredPackages)

#Load all required packages
apapp.pm.loadRequiredPackages(requiredPackages = apapp.pm.requiredPackages)

#Source all required files from github
apapp.pm.sourceNeededFilesFromRepo(useCpp = appSettings$useCpp)

#Start the webapp
aCOGARCH_SimulationApp(useHighCharts = appSettings$useHighcharts,
                       useCpp = appSettings$useCpp, 
                       portToRun = appSettings$appPort)
