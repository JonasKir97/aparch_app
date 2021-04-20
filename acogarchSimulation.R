#SIMULATIONSAPP----
aCOGARCH_SimulationApp <- function() {
  
  #Sidebar----
  aCOGARCH_SimulationApp_UI <- fluidPage(
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
                         shiny::p("DATENIMPORT: ein paar Zeitreihen vorverarbeiten und hier auswählbar machen.")
        )
      ),
      #Mainpanel----
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Simulation",
                   uiOutput(outputId = "simulationPlotUI")
          ),
          tabPanel(title = "Schätzung",
                   shiny::p("Schätzung")
          ),
          id = "mainTabPanel", 
          type = "pills"
        )
      )
    )
  )
  
  #Server----
  aCOGARCH_SimulationApp_Server <- function(input,output,session) {
    
    #SIMULATION----
    #_TEX-Formel-UIs----
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
      print(paste0("rendering: ",texToRender))
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
        #Plots Diskret: Noises, Volatilität, Y
        
        #Usereingaben validieren und in passende Form bringen
        discreteSimulationParameterList <- validateAndProcessDiscreteSimulationInput(shinyInputObject = input)
        if(!is.null(discreteSimulationParameterList$errorText)) {
          warning(discreteSimulationParameterList$errorText)
          return()
        }
        
        dv <- discreteSimulationParameterList$deltaVec
        gv <- discreteSimulationParameterList$gammaVec
        
        theta <- discreteSimulationParameterList$theta
        alpha <- discreteSimulationParameterList$alpha
        beta <- discreteSimulationParameterList$beta
        
        steps <- discreteSimulationParameterList$steps
        
        noises <- rnorm(n = steps, mean = 0,sd = 1)
        
        noisePlotData <- data.frame(x=1:steps,y=noises)
        noisePlot <- ggplot() + geom_line(data = noisePlotData, mapping = aes(x=x,y=y), col = "white")
        noisePlot <- noisePlot + labs(x = "Index", y = "Noises", title = "Verlauf der Noises")
        noisePlot <- noisePlot + theme(plot.title = element_text(hjust = 0.5))
        
        simulationDataList <- lapply(dv, function(delta) {
          lapply(gv, function(gamma) {
            simulationData <- simulateDiscreteAPARCH11(steps = steps, alpha = alpha, beta = beta,
                                                       theta = theta, gamma = gamma, delta = delta, 
                                                       fixedNoises = noises, useCPP = TRUE)
            data.frame(x = 1:length(simulationData$sigmaDelta),
                       sigmaDelta = simulationData$sigmaDelta, 
                       Y = simulationData$Y,
                       Simulation = paste0("Delta=",delta,",Gamma=",gamma), 
                       stringsAsFactors = FALSE)
          })
        })
        
        simulationPlotData <- do.call("rbind",do.call("c",simulationDataList))
        
        yPlot <- ggplot() +  theme(plot.title = element_text(hjust = 0.5))
        yPlot <- yPlot + geom_line(data = simulationPlotData, mapping = aes(x=x,y=Y,col=Simulation))
        yPlot <- yPlot + labs(x = "Index", y = "Y", title = "Verlauf des Prozesses Y")
        
        yPlot <- yPlot + theme(strip.text.x = element_blank(),
                               strip.background = element_rect(colour="white", fill="white"),
                               legend.position=c(0.8,0.9))
        
        sigmaDeltaPlot <- ggplot() + theme(plot.title = element_text(hjust = 0.5))
        sigmaDeltaPlot <- sigmaDeltaPlot + geom_line(data = simulationPlotData, mapping = aes(x=x,y=sigmaDelta,col=Simulation))
        sigmaDeltaPlot <- sigmaDeltaPlot + labs(x = "Index", y = "sigma^delta", title = "Verlauf des Prozesses sigma^delta")
        
        sigmaDeltaPlot <- sigmaDeltaPlot + theme(strip.text.x = element_blank(),
                                                 strip.background = element_rect(colour="white", fill="white"),
                                                 legend.position=c(0.8,0.9))
        
        output$noisePlot <- renderPlot(noisePlot + darkPlotTheme())
        output$yPlot <- renderPlot(yPlot + darkPlotTheme())
        output$sigmaDeltaPlot <- renderPlot(sigmaDeltaPlot + darkPlotTheme())
        
        simulationPlotUI <- tagList(
          plotOutput(outputId = "noisePlot"),
          plotOutput(outputId = "yPlot"),
          plotOutput(outputId = "sigmaDeltaPlot")
        )
        
      } else if(input$simulationType == "Stetig") {
        #Plots Stetig: Levyprozess L, Sprünge DeltaL, Volatitlität sigma, Returns G
        simulationPlotUI <- tagList(
          shiny::p(" Stetige Simulation")
        )
        
      } else { #sollte eigentlich nie passieren, da radioButtons fix
        warning("Ungültige Simulationsart ausgewählt!")
      }
      
      output$simulationPlotUI <- renderUI(simulationPlotUI)
    })
    
  }
  
  #Tags und Styling----
  additionalTags <- c("#specificationErrorText{color:white;font-size:14px;background-color:red}",
                      "#simulationErrorText{color:white;font-size:14px;background-color:red}",
                      "#calculateSimulation{background-color:#df691a}")
  
  aCOGARCH_SimulationApp_UI <- fluidPage(
    tags$head(tags$title("Kolloquium")),
    tags$style(HTML(additionalTags)),
    shiny::withMathJax(),
    useShinyjs(),
    theme = shinytheme("superhero"),
    h1c("Simulation und Schätzung des ACOGARCH"),
    aCOGARCH_SimulationApp_UI
  )
  
  #Start der App im Browser auf localhost:2021----
  shinyApp(ui = aCOGARCH_SimulationApp_UI, 
           server = aCOGARCH_SimulationApp_Server, 
           options = list(launch.browser = TRUE, host = "127.0.0.1", port = 2021))
}
