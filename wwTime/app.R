library(shiny)
library(dplyr)


# starting data
TimeStamp <- NULL


#data fetch and light processing function
getData <- function(){
  file_url <- "https://github.com/AnehEf/dev_repo/blob/main/DashboardData.RData?raw=true"
  load(url(file_url))
  TimeStamp <<- TimeStamp
}



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
      getData()
    }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
    
    
    output$UpdateTime <- renderText({
      reactiveGetData()
      format(TimeStamp)
    })
}


shinyApp(ui = ui, server = server)
