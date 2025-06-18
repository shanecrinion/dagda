library(shiny)

ui <- fluidPage(
  h2("Coming Soon"),
  p("This Shiny app is under construction.")
)

server <- function(input, output, session) {}

shinyApp(ui, server)
