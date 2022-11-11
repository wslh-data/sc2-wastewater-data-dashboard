library(shiny)
library(leaflet)
library(leaflet.minicharts)
library(dplyr)
library(forcats)
'%!in%' <- function(x,y)!('%in%'(x,y)) 


file_url <- "http://github.com/wslh-data/sc2-wastewater-data-dashboard/blob/main/data/DashboardData.RData?raw=true"


Cols<-c("All variants",
        names(freyja.map)[which(names(freyja.map) %!in% c("sites", "Date", "lat", "long", "Week", "Month"))])

basemap <- leaflet(width = "100%",  height = "100%") %>%
  addProviderTiles(providers$CartoDB.Positron)



ui <- fluidPage(

  leafletOutput("map", height=600),
  tags$style(type = "text/css", "html, body {width:100%; height: 100%}"),
  absolutePanel(top = 10, right = "10%",
                selectInput("variant", "Select a group of variants:", Cols, multiple = FALSE, selected = "All variants")
  )
  
)


server <- function(input, output, session){
  
  
  reactiveGetData <- reactive({
    load(url(file_url))
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  
  
  # Initialize map
  output$map <- renderLeaflet({
    basemap %>%
      addMinicharts(
        freyja.map$long, freyja.map$lat,
        layerId = freyja.map$sites,
        width = 30, height = 30
      )
  })
  
  
  # Update charts each time input value changes
  observe({
    if (input$variant == "All variants") {
      data <- freyja.map[, names(freyja.map)[which(names(freyja.map) %!in% c("sites", "Date", "lat", "long", "Week", "Month"))]]
    } else {
      data <- freyja.map[, input$variant]
    }
    maxValue <- max(as.matrix(data))
    
    leafletProxy("map", session) %>%
      updateMinicharts(
        freyja.map$sites,
        chartdata = data,
        colorPalette = colors.plot.map,
        maxValues = maxValue,
        timeFormat = "%b %Y",
        legendPosition = "bottomleft",
        initialTime = max(freyja.map$Month),
        time = freyja.map$Month,
        transitionTime = 900,
        type = ifelse(input$variant == "All variants", "pie", "polar-radius"),
        showLabels = input$labels
      )
  })
  
}


shinyApp(ui, server)
