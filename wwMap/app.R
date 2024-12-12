library(shiny)
library(leaflet)
library(leaflet.minicharts)
library(dplyr)
library(shinycssloaders)

# starting data
freyja.map <- NULL
colors.plot.map <- NULL
freyja.1st.occurence <-NULL


#data fetch and light processing function
getData <- function(){
  file_url <- "http://github.com/wslh-data/sc2-wastewater-data-dashboard/blob/main/data/DashboardData.RData?raw=true"
  load(url(file_url))
  selectionChoices <<- c("All variants", names(freyja.map  %>% select(-sites, -lat, -long, -Month)))
  freyja.map <<- freyja.map
  colors.plot.map <<- colors.plot.map
  freyja.1st.occurence <<- freyja.1st.occurence
}


basemap <- leaflet(width = "100%",  height = "100%", options = leafletOptions(zoomControl=TRUE, minZoom=7, maxZoom=9)) %>%
  addProviderTiles(providers$CartoDB.Positron)  %>%
  setView(lng=-89.9941,lat=44.6243, zoom=7) %>%
  fitBounds(-92.7, 42.56, -87.7, 46.8) %>%      #fitBounds(min(freyja.map$long), min(freyja.map$lat), max(freyja.map$long), max(freyja.map$lat))
  setMaxBounds(-95.2, 40.06, -84.2, 49.3)    #setMaxBounds(min(freyja.map$long) - 2, min(freyja.map$lat) - 2, max(freyja.map$long) + 2, max(freyja.map$lat) + 2)



ui <- fluidPage(

  leafletOutput("map", height=600) %>% withSpinner(color="#c5050c"),
  tags$style(type = "text/css", "html, body {width:100%; height: 100%}"),
  absolutePanel(top = 10, right = "10%",
                selectizeInput("variant", "Select a group of variants:", choices="All variants", multiple = FALSE)
  )
  
)



server <- function(input, output, session){
  
  # Refresh the data daily     
  reactiveGetData <- reactive({
    getData()
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  

  # Initialize map
  output$map <- renderLeaflet({
    reactiveGetData()
    updateSelectizeInput(session, "variant", choices=selectionChoices, server=TRUE, selected = "All variants")
    basemap %>%
      addMinicharts(
        freyja.map$long, freyja.map$lat,
        layerId = freyja.map$sites,
        width = 30, height = 30
      )
  })
  
  
  # Update charts each time input value changes
  observe({

    inc_lineages <- input$variant
    
    if (inc_lineages == "All variants") {
      data <- freyja.map  %>% select(-sites, -lat, -long, -Month)
      date.init<-max(freyja.map$Month)
    } else {
      data <- freyja.map %>% select(any_of(inc_lineages)) 
      date.init<-freyja.1st.occurence %>% filter(Lineage == inc_lineages) %>% select(Date)
      #date.init<-date.init[1, 1]
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
        initialTime = date.init,
        time = freyja.map$Month,
        transitionTime = 900,
        type = ifelse(inc_lineages == "All variants", "pie", "polar-radius"),
        showLabels = input$labels
      )
  })
  
}


shinyApp(ui, server)
