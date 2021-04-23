#SIMULATIONSAPP----
aCOGARCH_SimulationApp <- function() {
  
  useHighCharts <- TRUE
  
  if(useHighCharts) setGermanHighChartOptions()
  
  #Tags und Styling----
  txtTags <- c("#specificationErrorText{color:white;font-size:14px;background-color:red}",
               "#simulationErrorText{color:white;font-size:14px;background-color:red}",
               "#levySimulationError{color:white;font-size:14px;background-color:red}")
  btnTags <- c( "#calculateSimulation{background-color:#df691a}",
                "#simulateLevy{background-color:#df691a}")
  additionalTags <- c(txtTags,btnTags)
  
  #Sidebar----
  aCOGARCH_SimulationApp_UI <- uiWrapper(
    additionalTags,
    uiElement = fluidPage(
      sidebarLayout(
        sidebarPanel(
          #_Simulation----
          conditionalPanel(condition = "input.mainTabPanel == 'Simulation' ",
                           h1c("Simulation"),
                           radioButtons(inputId = "simulationType", label = "Simulationsart", 
                                        choices = c("Diskret","Stetig"), inline = TRUE),
                           uiOutput(outputId = "modelFormulaUI"),
                           conditionalPanel(condition = "input.simulationType == 'Diskret'",
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
                           conditionalPanel(condition = "input.simulationType == 'Stetig'",
                                            selectizeInput(inputId = "drivingLevyType", label = "Art des treibenden Lévyprozesses",
                                                           choices = c("compound Poisson","Varianz-Gamma","Brownsche Bewegung")),
                                            uiOutput(outputId = "drivingLevySpecificationUI")
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
                           verbatimTextOutput("levySimulationError")
          )
        ),
        #Mainpanel----
        mainPanel(
          tabsetPanel(
            tabPanel(title = "Simulation",
                     uiOutput(outputId = "simulationPlotUI")
            ),
            tabPanel(title = "Schätzung",
                     uiOutput(outputId = "estimationDataPlotUI"),
            ),
            tabPanel(title = "Lévysimulation",
                     uiOutput(outputId = "levySimulationMainUI"),
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
    #________________------
    
    #APARCH-SIMULATION----
    #_TEX-Formel-UI----
    discreteSimulationTexString <- paste0(
      "$$ Y_i = \\varepsilon_i \\sigma_i$$",
      "$$ \\sigma_i^\\delta = \\Theta + \\alpha h(Y_{i-1}) + \\beta \\sigma_{i-1}^\\delta $$",
      "$$ \\varepsilon_i \\sim \\mathcal{N}(0,1)$$"
    )
    
    continouSimulationTexString <- paste0(
      "$$ dG_t = \\sigma_t dL_t $$",
      "$$ \\sigma_t^\\delta = \\left( \\Theta \\int_0^t e^{X_s} ds + \\sigma_0^\\delta \\right) e^{-X_{t^-}} $$"
    )
    
    output$modelFormulaUI <- renderUI({
      texToRender <- if(input$simulationType == "Diskret") discreteSimulationTexString else continouSimulationTexString
      shiny::withMathJax(texToRender)
    })
    
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
      
      if(input$simulationType == "Diskret") {
        #__Diskret----
        #Plots Diskret: Noises, Volatilität, Y
        
        #Usereingaben validieren und in passende Form bringen
        discreteSimulationParameterList <- validateAndProcessDiscreteSimulationInput(shinyInputObject = input)
        output$simulationErrorText <- renderText(discreteSimulationParameterList$errorText)
        
        if(!is.null(discreteSimulationParameterList$errorText)) {
          warning(discreteSimulationParameterList$errorText)
          return()
        }
        
        steps <- discreteSimulationParameterList$steps
        
        noises <- rnorm(n = steps, mean = 0,sd = 1)
        
        simulationPlotData <- calculateDiscreteSimulationPlotData(discreteSimulationParameterList = discreteSimulationParameterList, 
                                                                  noises = noises)
        
        if(useHighCharts) {#Plots as Highcharts
          noisePlot <- singleLineHighchart(x = 1:steps, y = noises, plotTitle = "Verlauf der Noises", 
                                           xLabel = "Index", yLabel = "Verlauf der Noises")
          
          yPlot <- groupedLineHighchart(plotData = simulationPlotData, xColumn = "x", yColumn = "Y", colorColumn = "Simulation",
                                        xLabel = "Index", yLabel = "Y", plotTitle = "Verlauf des Prozesses Y")
          
          sigmaDeltaPlot <- groupedLineHighchart(plotData = simulationPlotData, xColumn = "x", yColumn = "sigmaDelta", 
                                                 colorColumn = "Simulation", xLabel = "Index", yLabel = "sigma^delta", 
                                                 plotTitle = "Verlauf des Prozesses sigma^delta")
          plotRenderer <- renderHighchart
          plotPutter <- highchartOutput
        } else { #Plots as ggplots
          noisePlot <- basicLinePlot(x = 1:steps, y = noises, plotTitle = "Verlauf der Noises",
                                     xLabel = "Index", yLabel = "Verlauf der Noises")
          
          yPlot <- basicMultiLinePlot(plotData = simulationPlotData, xColumn = "x", yColumn = "Y", colorColumn = "Simulation",
                                      xLabel = "Index", yLabel = "Y", plotTitle = "Verlauf des Prozesses Y")
          yPlot <- yPlot + theme(legend.position=c(0.8,0.9))
          
          sigmaDeltaPlot <- basicMultiLinePlot(plotData = simulationPlotData, xColumn = "x", yColumn = "sigmaDelta", colorColumn = "Simulation",
                                               xLabel = "Index", yLabel = "sigma^delta", plotTitle = "Verlauf des Prozesses sigma^delta")
          sigmaDeltaPlot <- sigmaDeltaPlot + theme(legend.position=c(0.8,0.9))
          plotRenderer <- renderPlot
          plotPutter <- plotOutput
        }
        
        output$noisePlot <- plotRenderer(noisePlot)
        output$yPlot <- plotRenderer(yPlot)
        output$sigmaDeltaPlot <- plotRenderer(sigmaDeltaPlot)
        
        simulationPlotUI <- tagList(
          withSpinner(plotPutter(outputId = "noisePlot")),
          withSpinner(plotPutter(outputId = "yPlot")),
          withSpinner(plotPutter(outputId = "sigmaDeltaPlot"))
        )
        
      } else if(input$simulationType == "Stetig") {
        #__Stetig----
        #Plots Stetig: Levyprozess L, Sprünge DeltaL, Volatitlität sigma, Returns G
        simulationPlotUI <- tagList(
          shiny::p(" Stetige Simulation")
        )
        
      } else { #sollte eigentlich nie passieren, da radioButtons fix
        warning("Ungültige Simulationsart ausgewählt!")
      }
      
      output$simulationPlotUI <- renderUI(simulationPlotUI)
    })
    
    
    #________________------
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
    
    #________________------
    #LEVYSIMULATION-----
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
          numericInput(inputId = "levySimuBBnr", label = "Anzahl innerhalb des Zeitgitters", value = 1000),
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
      
      if(!is.null(simulationSpecs$error)) {
        return()
      }
      
      timeGrid <- simulationSpecs$timeGrid
      simuType <- simulationSpecs$simuType
      
      if(simuType == "Compound Poisson") {
        #__Compound Poisson----
        levyData <- simulateCompoundPoisson(timeGrid = timeGrid, lambda = simulationSpecs$lambda, randomSeed = sample(1:10000,1))
        
        cpPlots <- generateCompoundPoissonLevyProcessPlot(levyData, useHighCharts = useHighCharts)
        
        if(useHighCharts) {
          output$processAndJumpPlot <- renderHighchart(cpPlots[["hc"]])
          levySimulationMainUI <- withSpinner(highchartOutput(outputId = "processAndJumpPlot", height = 800))
        } else {
          output$processPlot <- renderPlot(cpPlots[["processPlot"]])
          output$jumpPlot <- renderPlot(cpPlots[["jumpPlot"]])
          
          levySimulationMainUI <- tagList(
            withSpinner(plotOutput(outputId = "processPlot", height = 400)),
            withSpinner(plotOutput(outputId = "jumpPlot", height = 400))
          )
        }
        
      } else if(simuType == "Varianz-Gamma") {
        #__Varianz-Gamma----
        levyData <- simulateVarianceGamma(timeGrid = timeGrid, sigma = simulationSpecs$sigma, nu = simulationSpecs$nu,
                                          theta = simulationSpecs$theta, gs = simulationSpecs$gs, randomSeed = sample(1:10000,1))
        
        vgPlot <- generateVarianceGammaLevyProcessPlot(levyData, useHighCharts = useHighCharts)
        
        if(useHighCharts) {
          output$vgPlot <- renderHighchart(vgPlot)
          levySimulationMainUI <- withSpinner(highchartOutput(outputId = "vgPlot", height = 800))
        } else {
          output$vgPlot <- renderPlot(vgPlot)
          levySimulationMainUI <- withSpinner(plotOutput(outputId = "vgPlot", height = 800))
        }
      } else if (simuType == "Brownsche Bewegung") {
        #__Brownsche Bewegung----
        levyData <- simulateBrownianMotion(timeGrid = timeGrid, mu = simulationSpecs$mu, sigma = simulationSpecs$sigma,
                                           gs = simulationSpecs$gs, randomSeed = sample(1:10000,1))
        
        bbPlot <- generateBrownianLevyProcessPlot(levyData, useHighCharts = useHighCharts)
        
        if(useHighCharts) {
          output$bbPlot <- renderHighchart(bbPlot)
          levySimulationMainUI <- withSpinner(highchartOutput(outputId = "bbPlot", height = 800))
        } else {
          output$bbPlot <- renderPlot(bbPlot)
          levySimulationMainUI <- withSpinner(plotOutput(outputId = "bbPlot", height = 800))
        }
      } else {
        warning("Noch nicht implementiert")
        return()
      }
      
      output$levySimulationMainUI <- renderUI(levySimulationMainUI)
    })
    
  } #end server
  
  #Start der App im Browser auf localhost:2021----
  shinyApp(ui = aCOGARCH_SimulationApp_UI, 
           server = aCOGARCH_SimulationApp_Server, 
           options = list(launch.browser = TRUE, host = "127.0.0.1", port = 2021))
}