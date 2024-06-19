library(shiny)
library(dplyr)
library(plotly)
library(shinycssloaders)

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
  radioButtons('choice_display', "Proportion of variants normalized to:", choices = c("100% (default)", "SARS-CoV-2 wastewater level"), inline = TRUE),
  withSpinner(plotlyOutput("graph", height='100%'), color="#c5050c") 
  
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
    updateSelectizeInput(session, "choice", choices=selectionChoices, server=TRUE, selected = "All cities combined")
  })
  
  
  output$graph <- renderPlotly({
    
    reactiveGetData()

plot_ly(freyja.barplot %>% filter(City == input$choice) %>% mutate(Y = case_when(input$choice_display == "100% (default)" ~ proportion, 
                                                                                     input$choice_display != "100% (default)" ~proportion/100 * sars.per.week)),
            x = ~Date,
            y = ~Y,
            color = ~Lineage,
            colors = colors.plot.barplot,
            type = "bar",
            hovertext = ~hoover,
            hoverinfo = 'text') %>%
      
      layout(barmode = "stack",
             xaxis = list(title = ''),
             yaxis = list(title = ~ifelse(input$choice_display == "100% (default)", 'Proportion of variants (%)', 'Proportion of variants normalized to the <b>amount of\nSARS-CoV-2 in wastewater</b> (gene copies/person/day)')),
             legend = list(title=list(text='<b> Groups of variants </b>')),
             clickmode = "none")
  })
  
}

shinyApp(ui, server)
