#' helper for a nice centered HTML h1-title
h1c <- function(h1Title) return(HTML("<H1 align='center'>",h1Title,"</H1>"))

uiWrapper <- function(additionalTags, uiElement) {
  return(fluidPage(
    tags$head(tags$title("Kolloquium")),
    tags$style(HTML(additionalTags)),
    shiny::withMathJax(),
    useShinyjs(),
    theme = shinytheme("superhero"),
    h1c("Simulation und Schätzung des ACOGARCH"),
    uiElement
  ))
}