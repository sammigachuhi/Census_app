#load the requisite packages
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

#load the census data

load('counties.RData')
attach(counties)

#define UI ---------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c('County' = 'name')
      ),
      selectInput(
        inputId = "y",
        label = "Y-axis",
        choices = c('Male', 'Female', 'Intersex', 'Total')
      ),
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c('County' = 'name', 'Male', 'Female', 'Intersex', 'Total')
      ),
      downloadButton(
        outputId = "stat_download", label = "Download census statistics"
      )
    ),
    mainPanel(plotOutput(outputId = "census_statistics"),
              dataTableOutput(outputId = "census_table"))
  )
)

#define the server --------------------------------
server <- function(input, output, session){
  
  census_data <- reactive({
    counties %>%
      select(input$x, input$y)
  })
  
  output$census_statistics <- renderPlot({
    ggplot(census_data(), aes_string(x=input$x, y=input$y, color = input$z)) + 
      geom_point(size = 2) + theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1, vjust = 1))
  })
  
  output$census_table <- renderDT(
    
    census_data(), options = list(pageLength = 5), rownames = F
  )
  
  output$stat_download <- downloadHandler(
    filename = function(){
      paste("data-", ".csv", sep = "")
    },
    content = function(file){
      write.csv(census_data(), file)
    }
  )
}


#create the shiny app
shinyApp(ui, server)










