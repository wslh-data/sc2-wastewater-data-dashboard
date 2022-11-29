library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(viridis)


# starting data
freyja.heatmap <- NULL
colors.plot.heatmap <- NULL


#data fetch and light processing function
getData <- function(){
  file_url <- "http://github.com/wslh-data/sc2-wastewater-data-dashboard/blob/main/data/DashboardData.RData?raw=true"
  load(url(file_url))
  selectionChoices<<-levels(fct_relevel(unique(freyja.heatmap$predominance), "Predominant variants"))
  freyja.heatmap <<- freyja.heatmap
  colors.plot.heatmap <<-colors.plot.heatmap
}



ui <- fluidPage(
  
  selectizeInput("choice", "Select a group of variants:", choices = "Predominant variants", multiple = FALSE),
  radioButtons(inputId = "display",
               label = "Cities sorted by:",
               choices = c("alphabetical order", "population size"),
               selected = "alphabetical order"),
  
  plotlyOutput("graph")
  
)



server <- function(input, output, session){
  
  # Refresh the data daily
  reactiveGetData <- reactive({
    getData()
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  
  
  # Refresh the items in menu
  observe({
    reactiveGetData()
    updateSelectizeInput(session, "choice", choices=selectionChoices, server=TRUE, selected = "Predominant variants")
  })
  
  
  output$graph <- renderPlotly({
 
    reactiveGetData()
    
    if (input$choice == "Predominant variants") {
      
      ggplotly(
        
        ggplot() +
          
          {if(input$display == "alphabetical order")
            geom_tile(data = freyja.heatmap %>%
                        filter(predominance == "Predominant variants") %>%
                        mutate(sites = forcats::fct_reorder(sites, desc(sites))),
                      aes(x=as.Date(Date), y=sites, fill=Lineage, text = tooltip), color = "white", linetype = 1, linewidth = 0.2) } +
          
          {if(input$display == "population size")
            geom_tile(data = freyja.heatmap %>%
                        filter(predominance == "Predominant variants") %>%
                        mutate(sites = forcats::fct_reorder(sites, -desc(PopulationServed)),
                               Lineage = as.factor(Lineage)),
                      aes(x=as.Date(Date), y=sites, fill=Lineage, text = tooltip), color = "white", linetype = 1, linewidth = 0.2)} +
          
          theme_minimal() +
          theme(axis.text.x = element_text(angle=45, size = 8),
                legend.title = element_text()) +
          scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
          scale_fill_manual(values= colors.plot.heatmap) +
          labs(x="", y="", fill = "Groups of Variants:"),
        
        
        tooltip="text") %>%
        
        layout(autosize=FALSE)
      
    } else {
      
      ggplotly(
        
        ggplot() +
          
          {if(input$display == "alphabetical order")
            geom_tile(data = freyja.heatmap %>%
                        filter(predominance == input$choice) %>%
                        mutate(sites = forcats::fct_reorder(sites, -desc(sites))),
                      aes(x=as.Date(Date), y=sites, fill=`Relative abundance (%)`, text = tooltip), color = "white", linetype = 5, linewidth = 0.1) } +
          
          {if(input$display == "population size")
            geom_tile(data = freyja.heatmap %>%
                        filter(predominance == input$choice) %>%
                        mutate(sites = forcats::fct_reorder(sites, -desc(PopulationServed)),
                               Lineage = as.factor(Lineage)),
                      aes(x=as.Date(Date), y=sites, fill=`Relative abundance (%)`, text = tooltip), color = "white", linetype = 5, linewidth = 0.1)} +
          
          theme_minimal() +
          theme(axis.text.x = element_text(angle=45, size = 8),
                legend.title = element_text()) +
          scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
          scale_fill_viridis(direction = -1, limits=c(0,100)) +
          ylab("") +  xlab(""),
        
        
        tooltip="text") %>%
        
        layout(autosize=FALSE)
    }

  })
 
}


shinyApp(ui, server)

