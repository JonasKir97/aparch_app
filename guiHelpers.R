#' helper for a nice centered HTML h1-title
h1c <- function(h1Title) return(HTML("<H1 align='center'>",h1Title,"</H1>"))

uiWrapper <- function(additionalTags, uiElement) {
  return(fluidPage(
    tags$head(tags$title("Kolloquium")),
    tags$style(type = 'text/css', ".selectize-input { font-size: 16px; line-height: 16px;} .selectize-dropdown { font-size: 16px; line-height: 16px; }"),
    tags$style(HTML(additionalTags)),
    shiny::withMathJax(),
    #useShinyjs(),
    theme = shinytheme("superhero"),
    h1c("Simulation und Schätzung des ACOGARCH"),
    uiElement
  ))
}