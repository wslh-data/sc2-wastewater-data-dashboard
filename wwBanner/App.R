library(shiny)
library(dplyr)


# starting data
Banner <- NULL


#data fetch and light processing function
getData <- function(){
  file_url <- "http://github.com/wslh-data/sc2-wastewater-data-dashboard/blob/main/data/DashboardData.RData?raw=true"
  load("DashboardData.RData")
  Banner <<- Banner
}



ui <- fluidPage(

  fluidRow(
    column(12, htmlOutput("UpdateBanner", container = span), style="background-color:#ffecb5")
  )
  
)



server <- function(input, output, session) {
    
    reactiveGetData <- reactive({
      getData()
    }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
    
    
    output$UpdateBanner <- renderText({
      reactiveGetData()
      Banner
    })
}


shinyApp(ui = ui, server = server)
