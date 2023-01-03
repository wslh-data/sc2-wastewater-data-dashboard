library(shiny)
library(dplyr)
library(plotly)
library(forcats)
library(viridis)
library(shinycssloaders)


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
  
  selectizeInput("choice", "Select a group of variants:",selected="Predominant variants", choices = "Predominant variants", multiple = FALSE),
  radioButtons(inputId = "display",
               label = "Cities sorted by:",
               choices = c("alphabetical order", "population size"),
               selected = "alphabetical order"),
  
  withSpinner(plotlyOutput("graph"), color="#c5050c") 

)



server <- function(input, output, session){
  
  # Refresh the data daily
  reactiveGetData <- reactive({
    getData()
  }) %>% bindCache(format(Sys.time(),"%Y-%m-%d"))
  
  observe({
    reactiveGetData()
    updateSelectizeInput(session, "choice", choices=selectionChoices, server=TRUE, selected = "Predominant variants")
  })
  
  output$graph <- renderPlotly({
    reactiveGetData()
    
    if(input$choice == "Predominant variants"){

      # Define the colors
      n.color=2 * length(colors.plot.heatmap)
      df_colors = data.frame(range=c(0:(n.color-1)), colors=c(0:(n.color-1)))
      color_scale <- setNames(data.frame(df_colors$range, df_colors$colors), NULL)
      for (i in 1:n.color) {
        color_scale[[2]][[i]] <- colors.plot.heatmap[ceiling(i/2)]
        color_scale[[1]][[i]] <-  i / (n.color) - (i %% 2) / (n.color)
      }
      
      # Select data
      freyja.heatmap.subset<-freyja.heatmap %>% filter(predominance == "Predominant variants") %>%
        mutate(
          sites = forcats::fct_reorder(
            sites,
            if(input$display == "population size"){-desc(PopulationServed)}else{desc(sites)}))
      
      # Convert lineages into numeric variable
      lineage.color<-as.data.frame(1:length(colors.plot.heatmap))
      names(lineage.color)<-"num.lineage"
      lineage.color$Lineage<-sort(levels(factor(x = freyja.heatmap.subset$Lineage,
                                                levels = unique(freyja.heatmap.subset$Lineage))), 
                                  decreasing = TRUE)
      freyja.heatmap.subset<-left_join(freyja.heatmap.subset, lineage.color, by="Lineage")
      
      # Plot
      plot_ly(data = freyja.heatmap.subset, reversescale = T) %>%
        plotly::add_heatmap(x = ~(Date), 
                            y = ~sites,
                            z = ~num.lineage,
                            xgap = 0.5,
                            ygap = 0.5,
                            colorscale = color_scale,
                            colorbar = list(tickmode='array',
                                            title = "Groups of Variants",
                                            outlinewidth = 0,
                                            tickvals=c(1:length(colors.plot.heatmap)),
                                            ticklen = 6,
                                            ticktext=sort(levels(factor(x = lineage.color$Lineage)), decreasing=TRUE),
                                            len=0.5,
                                            tickfont = list(size = 0.7)),
                            text = ~tooltip,
                            hoverinfo ="text"
        ) %>% 
        layout(plot_bgcolor='white', 
               autosize=FALSE,
               xaxis = list(title = "",
                            tickangle = -45,
                            tickfont = list(size = 10),
                            tickformat = '%b %Y',
                            dtick = "M1"),
               yaxis = list(title = "", 
                            tickfont = list(size = 10),
                            dtick = "1"))
      
    } 
    else
      if(input$choice %in% selectionChoices){
        freyja.heatmap.subset <- freyja.heatmap %>% filter(predominance == input$choice) %>%
          mutate(
            sites = forcats::fct_reorder(
              sites, 
              if(input$display == "population size"){-desc(PopulationServed)}else{desc(sites)}))
        
        plot_ly(data = freyja.heatmap.subset, reversescale = T) %>%
          plotly::add_heatmap(x = ~(Date), 
                              y = ~sites,
                              z = ~`Relative abundance (%)`,
                              xgap = 0.5,
                              ygap = 0.5,
                              text = ~tooltip,
                              colorbar = list(tickmode='array',
                                              title = "Relative abundance (%)",
                                              cmin = 0,
                                              cmax = 100),
                              hoverinfo ="text"
          ) %>% 
          layout(plot_bgcolor='white', 
                 autosize=FALSE,
                 xaxis = list(title = "",
                              tickformat = '%b %Y',
                              tickfont = list(size = 10),
                              dtick = "M1"),
                 yaxis = list(title = "", 
                              tickfont = list(size = 10),
                              dtick = "1"))
      }
  }) 
}

shinyApp(ui, server)
