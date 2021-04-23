#' create a basic line plot with ggplot and fitting theme for the app
basicLinePlot <- function(x, y, plotTitle = NULL, xLabel = NULL, yLabel = NULL) {
  
  pd <- data.frame(x=x,y=y)
  p <- ggplot(pd) + geom_line(mapping = aes(x=x,y=y), col = "white")
  p <- p + labs(x = xLabel, y = yLabel, title = plotTitle)
  p <- p + darkPlotTheme()
  return(p)
}

basicMultiLinePlot <- function(plotData, xColumn, yColumn, colorColumn, 
                               xLabel = NULL, yLabel = NULL, legendTitle = colorColumn, plotTitle = NULL) {
  
  pd <- data.frame(x = plotData[[xColumn]], y = plotData[[yColumn]], col = plotData[[colorColumn]], stringsAsFactors = FALSE)
  
  p <- ggplot(pd) + geom_line(mapping = aes(x=x,y=y,col=col))
  p <- p + labs(x = xLabel, y = yLabel, title = plotTitle, color = legendTitle)
  p <- p + darkPlotTheme()
  return(p)
}

#' helper to set the theme of the plots for Shiny-App
darkPlotTheme <- function(axisTextSize = 10,
                          axisTitleSize = 12,
                          legendTextSize = 10,
                          legendTitleSize = 12,
                          plotTitleSize = 14,
                          legendkey_space = 1,
                          panel_space = 0.5,
                          backColor = "#49566b",
                          foreColor = "#2B3E50") {
  
  textColor <- "#C0C0C0"
  legendTextColor <- "#FFFFFF"
  elements <- list(axis.text.x = element_text(size = axisTextSize, color = textColor),
                   axis.text.y = element_text(size = axisTextSize, color = textColor),
                   axis.ticks = element_line(colour = backColor),
                   plot.background = element_rect(fill = backColor, color = backColor),
                   panel.background = element_rect(fill = foreColor, colour = foreColor),
                   panel.border = element_blank(),
                   panel.grid.major = element_line(colour = backColor, size = 0.5),
                   panel.grid.minor = element_blank(),
                   strip.background = element_rect(fill = textColor),
                   axis.line = element_line(colour = backColor, size = 0.5),
                   legend.key.height = unit(legendkey_space, "lines"),
                   plot.title = element_text(color = "white", size = plotTitleSize, hjust = 0.5, face = "bold"),
                   axis.title.x = element_text( color = textColor, size = axisTitleSize, face = "bold"),
                   axis.title.y = element_text( color = textColor, size = axisTitleSize, face = "bold"),
                   legend.text = element_text( color = legendTextColor, size = legendTextSize),
                   legend.title = element_text( color = legendTextColor, size = legendTitleSize, face = "bold"),
                   legend.key = element_rect(fill = backColor, colour = backColor),
                   legend.background = element_rect(fill = backColor),
                   panel.spacing = unit(panel_space, "lines"))
  structure(elements, class = c("theme", "gg"), complete = FALSE)
}

darkEmptyPlot <- function() {
  ggplot() + darkPlotTheme(backColor = "#2B3E50")
}

setGermanHighChartOptions <- function() {
  hcoptslang <- getOption("highcharter.lang")
  deutscheTage <- c("Sonntag","Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag")
  hcoptslang$weekdays <- deutscheTage
  deutscheMonate <- c("Januar","Februar","März","April","Mai","juni","Juli","August","September","Oktober","November","Dezember")
  hcoptslang$months <- deutscheMonate
  deutscheMonateShort <- c("Jan","Feb","März","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Dez")
  hcoptslang$shortMonths <- deutscheMonateShort
  hcoptslang$loading <- "Lade..."
  hcoptslang$noData <- "Keine Daten zum Anzeigen."
  hcoptslang$resetZoom <- "Zoom zurücksetzen"
  hcoptslang$printChart <- "Chart drucken"
  hcoptslang$drillUpText <- "Zurück zu {series.name}"
  hcoptslang$contextButtonTitle <- "Kontextmenü"
  hcoptslang$downloadJPEG <- "Download JPEG"
  hcoptslang$downloadPDF <- "Download PDF-Dokument"
  hcoptslang$downloadPNG <- "Download PNG"
  hcoptslang$downloadSVG <- "Download SVG"
  options(highcharter.lang = hcoptslang)
}


stockHighchartFromCleanNasdaqData <- function(cleanData, plotTitle = NULL, priceColor = "white", returnColor = "green") {
  
  xtsPrices <- xts::xts(x = cleanData$price, order.by = cleanData$day)
  xtsLogReturns <- xts::xts(x = cleanData$logReturn, order.by = cleanData$day)
  xtsReturns <- xts::xts(x = cleanData$Return, order.by = cleanData$day)
  
  hc <- highchart(type="stock") %>%
    hc_yAxis_multiples(create_yaxis(2, height = c(2, 1), turnopposite = TRUE)) %>% 
    hc_title(text = plotTitle, style=list(color="white"))
  
  hc <- hc %>% 
    hc_add_series(xtsPrices, name = "Preis", yAxis = 0, color = priceColor) %>% 
    hc_add_series(xtsLogReturns, name = "Log-Return", yAxis = 1, color = returnColor) %>% 
    hc_add_series(xtsLogReturns, name = "Return", yAxis = 1, color = returnColor)
  
  hc <- hc %>% hc_rangeSelector(enabled = FALSE)
  
  hc
}

addTitleAndLabelsToHighchart <- function(hc, plotTitle = NULL, xLabel = NULL, yLabel = NULL) {
  
  if(is.null(plotTitle)) plotTitle <- ""
  hc <- hc_title(hc, text = plotTitle,
                 align = "center", 
                 style = list(color = "white", useHTML = TRUE))
  
  if(!length(xLabel)) xLabel <- ""
  hc <- hc_xAxis(hc, 
                 title=list(text = xLabel, style=list(color="white")), 
                 labels=list(style=list(color="white")))
  
  if(!length(yLabel)) yLabel <- ""
  hc <- hc_yAxis(hc, 
                 title=list(text = yLabel, style=list(color="white")), 
                 labels=list(style=list(color="white")))
  
  return(hc)
}

groupedLineHighchart <- function(plotData, xColumn, yColumn, colorColumn, 
                               xLabel = NULL, yLabel = NULL, legendTitle = colorColumn, plotTitle = NULL) {
  
  pd <- data.frame(x = plotData[[xColumn]], y = plotData[[yColumn]], col = plotData[[colorColumn]], stringsAsFactors = FALSE)
  hc <- highcharter::hchart(pd,"line",hcaes(x = x, y = y, group = col), dataLabels = list(color="white"))
  hc <- addTitleAndLabelsToHighchart(hc = hc, plotTitle = plotTitle, xLabel = xLabel, yLabel = yLabel)
  hc <- hc_add_theme(hc, hc_theme(chart = list(backgroundColor = "#2B3E50")))
  hc
}

singleLineHighchart <- function(x,y,xLabel = NULL, yLabel = NULL,plotTitle = NULL) {
  pd <- data.frame(x=x,y=y)
  hc <- highcharter::hchart(pd,"line",hcaes(x = x, y = y), dataLabels = list(color="white"))
  hc <- addTitleAndLabelsToHighchart(hc = hc, plotTitle = plotTitle, xLabel = xLabel, yLabel = yLabel)
  hc <- hc_add_theme(hc, hc_theme(chart = list(backgroundColor = "#2B3E50")))
  hc
}

