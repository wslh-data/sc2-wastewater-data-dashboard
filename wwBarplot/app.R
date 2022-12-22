library(shiny)
library(dplyr)
library(plotly)


# starting data
freyja.barplot <- NULL
colors.plot.barplot <- NULL
TimeStamp <- NULL


#data fetch and light processing function
getData <- function(){
  file_url <- "http://github.com/wslh-data/sc2-wastewater-data-dashboard/blob/main/data/DashboardData.RData?raw=true"
  load(url(file_url))
  selectionChoices<<-sort(unique(as.factor(freyja.barplot$City)))
  freyja.barplot <<- freyja.barplot
  colors.plot.barplot <<-colors.plot.barplot
}




ui <- fluidPage(
  
  selectizeInput("choice", "Select a city:", choices = "All cities combined", multiple = FALSE),
  plotlyOutput("graph")
  
)


server <- function(input, output, session){
  
  # Refresh the data daily
  reactiveGetData <- reactive({
    getData()
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  
  # Refresh items in the menu
  observe({
    reactiveGetData()
    updateSelectizeInput(session, "choice", choices=selectionChoices, server=TRUE, selected = "All cities combined")
  })
  
  
  output$graph <- renderPlotly({
    
    reactiveGetData()

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
