#example

library(shiny)
library(shinycssloaders)
library(plotly)
library(lubridate)

ui <- fluidPage(
    fluidRow(
      column(width=12,
        plotlyOutput(outputId = "example")%>% withSpinner(color="#c5050c")
      )
    )
)

server <- function(input, output) {
  
  ##### Caching function to get data
  #reactiveGetData <- reactive({
    ##### Place function that fetches data
  #}) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))

  output$example <- renderPlotly({
    ##### get data here
    #data <- reactiveGetData()
    fig <- plot_ly()
    fig <- fig %>% add_trace(
      type = "scatter",
      x = mtcars$wt,
      y = mtcars$mpg,
      name = 'Car weight and MPG',
      mode = "markers"
    )
    fig <- fig %>%
      layout(
        autosize=TRUE
      )
    fig
  })
  
}
shinyApp(ui, server)