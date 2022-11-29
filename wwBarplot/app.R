library(shiny)
library(dplyr)
library(plotly)

file_url <- "http://github.com/wslh-data/sc2-wastewater-data-dashboard/blob/main/data/DashboardData.RData?raw=true"



ui <- fluidPage(
  
  selectInput("choice", "Select a city:", choices = sort(unique(freyja.barplot$City)), multiple = FALSE, selected = "All cities combined"),
  plotlyOutput("graph")
)


server <- function(input, output, session){
  
  
  
  reactiveGetData <- reactive({
    load(url(file_url))
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  
  
  output$graph <- renderPlotly({
    
    plot_ly(freyja.barplot %>% filter(City == input$choice),
            x = ~Date,
            y = ~proportion,
            color = ~Lineage,
            colors = colors.plot.barplot,
            type = "bar",
            hovertext = ~hoover,
            hoverinfo = 'text') %>%
      
      layout(barmode = "stack",
             xaxis = list(title = ''),
             yaxis = list(title = 'Proportion of variants (%)'),
             legend = list(title=list(text='<b> Groups of variants </b>')),
             clickmode = "none")
  })
  
}

shinyApp(ui, server)
