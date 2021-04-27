#SIMULATIONSAPP----
#' @param useHighCharts logical indicating whether to use highcharts instead of ggplot
#' @param useCpp logical indicating whether to use cpp-implmentations
#' @param portToRun port for the webapp to run on localhost
aCOGARCH_SimulationApp <- function(useHighCharts = TRUE, useCpp = TRUE, portToRun = 2021) {
  
  if(useHighCharts) setGermanHighChartOptions()
  
  #Tags und Styling----
  txtTags <- c("#specificationErrorText{color:white;font-size:14px;background-color:red}",
               "#simulationErrorText{color:white;font-size:14px;background-color:red}",
               "#levySimulationError{color:white;font-size:14px;background-color:red}")
  btnTags <- c("#calculateSimulation{background-color:#df691a}",
               "#simulateLevy{background-color:#df691a}",
               "#calculateFJA{background-color:#df691a}")
  additionalTags <- c(txtTags,btnTags)
  
  fjaHelpText <- paste0("Der Levyprozess kann nun durch eine First-Jump-Approximation angenähert werden. ",
                        "Dazu wird das Gesamtintervall in 10 Schritten in jeweils 2^k Intervalle aufgeteilt ",
                        "für k=1,...,10. Die Sprunghöhen m(k) sind die 10 Werte 1,0.9,0.8,...,0.1.")
  
  #Sidebar----
  aCOGARCH_SimulationApp_UI <- uiWrapper(
    additionalTags,
    uiElement = fluidPage(
      sidebarLayout(
        sidebarPanel(
          #_Simulation----
          conditionalPanel(condition = "input.mainTabPanel == 'Simulation' ",
                           h1c("Simulation"),
                           tabsetPanel(
                             id = "simulationTypePanel", 
                             tabPanel(title = "Diskrete Simulation",
                                      uiOutput("discreteSimulationModelFormulaUI"),
                                      numericInput(inputId = "simulationStepsDiscrete", label = "Länge der diskreten Simulation",
                                                   value = 1000, min = 100, max = NA),
                                      fluidRow(
                                        column(3,textInput(inputId = "deltaDiscrete", label = withMathJax("$$\\delta$$"), value = "2")),
                                        column(3,textInput(inputId = "gammaDiscrete", label = withMathJax("$$\\gamma$$"), value = "0")),
                                        column(2,numericInput(inputId = "thetaDiscrete", label = withMathJax("$$\\Theta$$"), value = 0.5)),
                                        column(2,numericInput(inputId = "alphaDiscrete", label = withMathJax("$$\\alpha$$"), value = 0.5)),
                                        column(2,numericInput(inputId = "betaDiscrete", label = withMathJax("$$\\beta$$"), value = 0.5))
                                      ),
                                      shiny::p("Die Folge der Noises wird gemäß einer N(0,1)-Verteilung erzeugt.")
                             ),
                             tabPanel(title = "Stetige Simulation",
                                      uiOutput("continuousSimulationModelFormulaUI"),
                                      selectizeInput(inputId = "drivingLevyType", label = "Art des treibenden Lévyprozesses",
                                                     choices = c("compound Poisson","Varianz-Gamma","Brownsche Bewegung")),
                                      uiOutput(outputId = "drivingLevySpecificationUI")
                             ), 
                             type = "pills"
                           ),
                           numericInput(inputId = "simulationSeed", label = "Seed für Simulationen (Reproduzierbarkeit)", value = 2021),
                           actionButton(input = "calculateSimulation", label = "Simulation berechnen", 
                                        icon = icon("chart-line"), width = "100%"),
                           shiny::p(""),
                           verbatimTextOutput(outputId = "simulationErrorText")
          ),
          #_Schätzung----
          conditionalPanel(condition = " input.mainTabPanel == 'Schätzung' ",
                           h1c("Schätzung"),
                           shiny::p("Hier wird eine Schätzung des ACOGARCH mittels PMLE durchgeführt."),
                           shiny::fileInput(inputId = "nasdaqCsvFile", label = "CSV Import (Dateien von nsdaq.com)",
                                            multiple = FALSE, accept = ".csv", placeholder = "csv-Datei auswählen.", 
                                            buttonLabel = "Import csv")
          ),
          #_Lévysimulation-----
          conditionalPanel(condition = " input.mainTabPanel == 'Lévysimulation' ",
                           h1c("Lévysimulation"),
                           selectizeInput(inputId = "levySimulationType", label = "Art des Lévyprozesses",
                                          choices = c("Compound Poisson","Varianz-Gamma","Brownsche Bewegung")),
                           textInput(inputId = "levySimuTimeGrid", label = "Zeitgitter der Simulation", value = "1:100"),
                           uiOutput(outputId = "levySimulationSpecificationUI"),
                           actionButton(inputId = "simulateLevy", label = "Simulieren", width = "100%"),
                           shiny::p(" "),
                           verbatimTextOutput("levySimulationError"),
                           shinyjs::hidden(div(id = "fjaInSimulationDiv", 
                                               tagList(
                                                 shiny::p(fjaHelpText),
                                                 actionButton(inputId = "calculateFJA", 
                                                              label = "First-Jump-Approximation berechnen", width = "100%")
                                               )))
          )
        ),
        #Mainpanel----
        mainPanel(
          style = paste0("overflow: auto;height: calc(100vh - 100px) !important"),
          tabsetPanel(
            tabPanel(title = "Simulation",
                     uiOutput(outputId = "simulationPlotUI")
            ),
            tabPanel(title = "Schätzung",
                     uiOutput(outputId = "estimationDataPlotUI"),
            ),
            tabPanel(title = "Lévysimulation",
                     uiOutput(outputId = "levySimulationMainUI"),
                     uiOutput(outputId = "levySimulationFJAUI")
            ),
            id = "mainTabPanel", 
            type = "pills"
          )
        )
      )
    )
  )
  
  #Server----
  aCOGARCH_SimulationApp_Server <- function(input,output,session) {
    #________________________________------
    
    #APARCH-SIMULATION----
    #_TEX-Formel-UI----
    output$discreteSimulationModelFormulaUI <- renderUI(shiny::withMathJax(paste0(
      "$$ Y_i = \\varepsilon_i \\sigma_i$$",
      "$$ \\sigma_i^\\delta = \\Theta + \\alpha h(Y_{i-1}) + \\beta \\sigma_{i-1}^\\delta $$",
      "$$ \\varepsilon_i \\sim \\mathcal{N}(0,1)$$"
    )))
    
    output$continuousSimulationModelFormulaUI <- renderUI(shiny::withMathJax(paste0(
      "$$ dG_t = \\sigma_t dL_t $$",
      "$$ \\sigma_t^\\delta = \\left( \\Theta \\int_0^t e^{X_s} ds + \\sigma_0^\\delta \\right) e^{-X_{t^-}} $$"
    )))
    
    #_drivingLevySpecificationUI----
    output$drivingLevySpecificationUI <- renderUI({
      levyType <- input$drivingLevyType
      if(levyType == "compound Poisson") {
        return(shiny::p("COMPOUND POISSON"))
      } else if(levyType == "Varianz-Gamma") {
        return(shiny::p("VARIANZ GAMMA"))
      } else if(levyType == "Brownsche Bewegung") {
        return(shiny::p("BROWNSCHE"))
      } else {
        output$specificationErrorText <- renderText("Ungültiger Lévyprozess ausgewählt.")
        return(verbatimTextOutput(outputId = "specificationErrorText"))
      }
    })
    
    #_Berechnung----
    observeEvent(input$calculateSimulation, {
      
      simulationSeed <- as.integer(input$simulationSeed)
      set.seed(simulationSeed)
      
      if(input$simulationTypePanel == "Diskrete Simulation") {
        #__Diskret----
        #Plots Diskret: Noises, Volatilität, Y
        
        #Usereingaben validieren und in passende Form bringen
        discreteSimulationParameterList <- parseDiscreteSimulationInput(shinyInputObject = input)
        output$simulationErrorText <- renderText(discreteSimulationParameterList$errorText)
        
        if(!is.null(discreteSimulationParameterList$errorText)) {
          warning(discreteSimulationParameterList$errorText)
          return()
        }
        
        steps <- discreteSimulationParameterList$steps
        
        noises <- rnorm(n = steps, mean = 0,sd = 1)
        
        a <- Sys.time()
        simulationPlotData <- calculateDiscreteSimulationPlotData(discreteSimulationParameterList = discreteSimulationParameterList, 
                                                                  noises = noises, 
                                                                  useCpp = useCpp)
        print(paste0("calculateDiscreteSimulationPlotData done in ", Sys.time()-a))
        
        discretePlots <- generateDiscreteSimulationPlots(simulationPlotData = simulationPlotData,
                                                         steps =  steps, 
                                                         noises = noises, 
                                                         useHighCharts = useHighCharts)
        
        #Render plots and generate fitting ui elements
        simulationPlotUI <- lapply(names(discretePlots$plots), function(nam) {
          output[[nam]] <- discretePlots$plotRenderer(discretePlots$plots[[nam]])
          withSpinner(discretePlots$plotPutter(outputId = nam))
        })
        
      } else if(input$simulationTypePanel == "Stetige Simulation") {
        #__Stetig----
        #Plots Stetig: Levyprozess L, Sprünge DeltaL, Volatitlität sigma, Returns G
        simulationPlotUI <- tagList(
          shiny::p(" Stetige Simulation")
        )
        
      } else { #sollte eigentlich nie passieren, da radioButtons fix
        stop("Ungültige Simulationsart ausgewählt!")
      }
      
      output$simulationPlotUI <- renderUI(simulationPlotUI)
    })
    
    
    #________________________________------
    #SCHÄTZUNG----
    #_csv-Import----
    observeEvent(input$nasdaqCsvFile, {
      
      if(is.null(input$nasdaqCsvFile)) {
        output$estimationOriginalDataPlot <- renderPlot(odp)
        return()
      }
      
      priceData <- nasdaqDataReader(input$nasdaqCsvFile$datapath)
      
      if(useHighCharts) {
        output$estimationOriginalDataChart <- renderHighchart({
          pltTitle <- paste0("Verlauf der Preise und Returns von ",input$nasdaqCsvFile$name)
          stockHighchartFromCleanNasdaqData(cleanData = priceData, plotTitle = pltTitle)
        })
        estimationDataPlotUI <- tagList(
          withSpinner(highchartOutput(outputId = "estimationOriginalDataChart", height = 600))
        )
        
      } else {
        pricePlot <- basicLinePlot(x = priceData$day, y = priceData$price,
                                   plotTitle = paste0("Verlauf der Preise von ",input$nasdaqCsvFile$name),
                                   xLabel = "Datum", yLabel ="Preis")
        
        returnPlotData <- data.frame(x = rep(priceData$day,2), 
                                     y = c(priceData$Return,priceData$logReturn), 
                                     Returnart = rep(c("Return","Log-Return"),each=NROW(priceData)), 
                                     stringsAsFactors = FALSE)
        
        returnPlot <- basicMultiLinePlot(plotData = returnPlotData, xColumn = "x", yColumn = "y", colorColumn = "Returnart", 
                                         xLabel = "Datum", yLabel = "Return", legendTitle = "Art der Returns", 
                                         plotTitle = paste0("Verlauf der Returns von ",input$nasdaqCsvFile$name))
        returnPlot <- returnPlot + theme(legend.position=c(0.8,0.9))
        
        output$estimationOriginalPricePlot <- renderPlot(pricePlot)
        output$estimationOriginalReturnPlot <- renderPlot(returnPlot)
        estimationDataPlotUI <- tagList(
          withSpinner(plotOutput(outputId = "estimationOriginalPricePlot")),
          withSpinner(plotOutput(outputId = "estimationOriginalReturnPlot"))
        )
        
      }
      
      output$estimationDataPlotUI <- renderUI(estimationDataPlotUI)
    })
    
    #________________________________------
    #LEVYSIMULATION-----
    currentLevySimulationData <- NULL
    
    observeEvent(input$levySimulationType, {
      output$levySimulationMainUI <- renderUI(NULL)
      if(useHighCharts) {
        output$processAndJumpPlot <- renderHighchart(NULL)
        output$vgPlot <- renderHighchart(NULL)
        output$bbPlot <- renderHighchart(NULL)
      } else {
        output$processPlot <- renderPlot(darkEmptyPlot())
        output$jumpPlot <- renderPlot(darkEmptyPlot())
        output$vgPlot <- renderPlot(darkEmptyPlot())
        output$bbPlot <- renderPlot(darkEmptyPlot())
      }
    })
    
    #_UI-----
    observeEvent(input$levySimulationType, {
      simuType <- input$levySimulationType
      
      if(simuType == "Compound Poisson") {
        levySimulationSpecificationUI <- tagList(
          numericInput(inputId = "levySimuCPlambda", label = "Rate Lambda der Interarrival times", value = 1),
          shiny::p("Die Sprünge werden erstmal nur als N(0,1)-verteilt simuliert. Das wird noch erweitert.")
        )
      } else if(simuType == "Varianz-Gamma") {
        
        levySimulationSpecificationUI <- tagList(
          fluidRow(
            column(3,numericInput(inputId = "levySimuVGsigma", label = "sigma", value = 1)),
            column(3,numericInput(inputId = "levySimuVGnu", label = "nu", value = 0.05)),
            column(3,numericInput(inputId = "levySimuVGtheta", label = "theta", value = 0.5)),
            column(3,numericInput(inputId = "levySimuVGgs", label = "Schrittweite", value = 0.01))
          )
        )
        
      } else if(simuType == "Brownsche Bewegung") {
        
        levySimulationSpecificationUI <- tagList(
          fluidRow(
            column(4, numericInput(inputId = "levySimuBBmu", label = "Mittelwert", value = 0)),
            column(4, numericInput(inputId = "levySimuBBsd", label = "Standardabweichung", value = 1)),
            column(4,numericInput(inputId = "levySimuBBgs", label = "Schrittweite", value = 0.01))
          )
        )
        
      } else {
        output$simuSpecificationErrorText <- renderText("Ungültiger Lévyprozess ausgewählt.")
        return(verbatimTextOutput(outputId = "simuSpecificationErrorText"))
      }
      
      output$levySimulationSpecificationUI <- renderUI(levySimulationSpecificationUI)
    })
    
    #_Calculate----
    observeEvent(input$simulateLevy, {
      simulationSpecs <- parseLevySimulationSpecification(shinyInput = input)
      output$levySimulationError <- renderText(simulationSpecs$error)
      
      output$levySimulationFJAUI <- renderUI(NULL)
      
      if(!is.null(simulationSpecs$error)) {
        shinyjs::hide("fjaInSimulationDiv")
        return()
      }
      
      timeGrid <- simulationSpecs$timeGrid
      simuType <- simulationSpecs$simuType
      
      if(simuType == "Compound Poisson") {
        levyData <- simulateCompoundPoisson(timeGrid = timeGrid, lambda = simulationSpecs$lambda, randomSeed = NULL)
        levyPlot <- generateCompoundPoissonLevyProcessPlot(levyData, useHighCharts = useHighCharts)
      } else if(simuType == "Varianz-Gamma") {
        levyData <- simulateVarianceGamma(timeGrid = timeGrid, sigma = simulationSpecs$sigma, nu = simulationSpecs$nu,
                                          theta = simulationSpecs$theta, gs = simulationSpecs$gs, randomSeed = NULL)
        levyPlot <- generateVarianceGammaLevyProcessPlot(levyData, useHighCharts = useHighCharts)
      } else if (simuType == "Brownsche Bewegung") {
        levyData <- simulateBrownianMotion(timeGrid = timeGrid, mu = simulationSpecs$mu, sigma = simulationSpecs$sigma,
                                           gs = simulationSpecs$gs, randomSeed = NULL)
        levyPlot <- generateBrownianLevyProcessPlot(levyData, useHighCharts = useHighCharts)
      }
      
      if(useHighCharts) {
        output$levyPlot <- renderHighchart(levyPlot)
        levySimulationMainUI <- withSpinner(highchartOutput(outputId = "levyPlot", height = 800))
      } else {
        levyPlot <- levyPlot + theme(legend.position=c(0.9,0.95))
        output$levyPlot <- renderPlot(levyPlot)
        levySimulationMainUI <- withSpinner(plotOutput(outputId = "levyPlot", height = 800))
      }
      
      currentLevySimulationData <<- levyData
      
      shinyjs::show("fjaInSimulationDiv")
      
      output$levySimulationMainUI <- renderUI(levySimulationMainUI)
    })
    
    #_FJA----
    observeEvent(input$calculateFJA, {
      ld <- currentLevySimulationData
      if(is.null(ld)) {
        return()
      }
      
      TT <- ld$jumpTimes[length(ld$jumpTimes)]
      N <- 2^(1:10)
      tns0 <- lapply(N, function(n2) seq(0,TT,length.out = n2+1))
      mns0 <- seq(1,0,length.out = length(N)+1)[-(length(N)+1)]
      
      fjaData <- firstJumpApproximation(levyData = ld, tns = tns0, mns = mns0)
      
      fjaPlots <- fjaData$fjaPlotList
      
      originalPlot <- basicLinePlot(x = ld$jumpTimes, y = ld$levyProcess, plotTitle = "Originalprozess", 
                                    xLabel = "Zeit", yLabel = "Originalprozess", lineColor = "white") + 
        scale_x_continuous(limits = c(ld$jumpTimes[1],ld$jumpTimes[length(ld$jumpTimes)]), expand = c(0,0))
      
      fjaPlots <- c(fjaPlots, list(originalPlot))
      
      plotIds <- paste0("fjaPlot_",1:length(fjaPlots))
      
      uiList <- lapply(1:length(fjaPlots), function(k) {
        output[[plotIds[k]]] <- renderPlot(fjaPlots[[k]])
        withSpinner(plotOutput(outputId = plotIds[k]))
      })
      
      uiList <- c(list(h1c("First-Jump-Approximation")),uiList)
      
      output$levySimulationFJAUI <- renderUI(uiList)
    })
    
    
    
  } #end server
  
  #________________________________------
  #Start der App im Browser auf localhost----
  shinyApp(ui = aCOGARCH_SimulationApp_UI, 
           server = aCOGARCH_SimulationApp_Server, 
           options = list(launch.browser = TRUE, host = "127.0.0.1", port = portToRun))
}