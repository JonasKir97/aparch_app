#SIMULATIONSAPP----
aCOGARCH_SimulationApp <- function() {
  
  useHighCharts <- TRUE
  
  setGermanHighChartOptions()
  
  #Tags und Styling----
  additionalTags <- c("#specificationErrorText{color:white;font-size:14px;background-color:red}",
                      "#simulationErrorText{color:white;font-size:14px;background-color:red}",
                      "#calculateSimulation{background-color:#df691a}")
  
  #Sidebar----
  aCOGARCH_SimulationApp_UI <- uiWrapper(
    additionalTags,
    uiElement = fluidPage(
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(condition = "input.mainTabPanel == 'Simulation' ",
                           #_Simulation----
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
          conditionalPanel(condition = " input.mainTabPanel == 'Schätzung' ",
                           #_Schätzung----
                           h1c("Schätzung"),
                           shiny::p("Hier wird eine Schätzung des ACOGARCH mittels PMLE durchgeführt."),
                           shiny::fileInput(inputId = "nasdaqCsvFile", label = "CSV Import (Dateien von nsdaq.com)",
                                            multiple = FALSE, accept = ".csv", placeholder = "csv-Datei auswählen.", 
                                            buttonLabel = "Import csv")
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
            id = "mainTabPanel", 
            type = "pills"
          )
        )
      )
    )
  )
  
  #Server----
  aCOGARCH_SimulationApp_Server <- function(input,output,session) {
    
    #SIMULATION----
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
    
    
    #csv-Import----
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
    
  } #end server
  
  #Start der App im Browser auf localhost:2021----
  shinyApp(ui = aCOGARCH_SimulationApp_UI, 
           server = aCOGARCH_SimulationApp_Server, 
           options = list(launch.browser = TRUE, host = "127.0.0.1", port = 2021))
}