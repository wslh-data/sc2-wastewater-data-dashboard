library(shiny)
library(dplyr)

file_url <- "http://github.com/wslh-data/sc2-wastewater-data-dashboard/blob/main/data/DashboardData.RData?raw=true"



ui <- fluidPage(

  basicPage(
    h4(
      "Last update: ",
      textOutput("UpdateTime", container = span)
    )
  )

)



server <- function(input, output, session) {
    
    reactiveGetData <- reactive({
        load(url(file_url))
        }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  
  
    output$UpdateTime <- renderText({
    format(TimeStamp)
    })
}


shinyApp(ui = ui, server = server)
