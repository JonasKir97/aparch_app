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