# Load packages -----------------------------------------------------------
library(shiny)
library(tidyverse)
library(SportsAnalytics)

# Load Source -------------------------------------------------------------
source("Modify_data.R")

# Define UI for application that draws a histogram
ui <- fluidRow(
   
   # Application title
   titlePanel("Manu Ginobili !!!"),
   
   # Output interactive data frame  
   fluidRow(
      column(width = 12, DT::dataTableOutput("df1"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    df_ginobili <- reactive({
      df_ginobili_2k_per
    })
    
    output$df1 <- DT::renderDataTable(df_ginobili())
    
}



# Run the application 
shinyApp(ui = ui, server = server)

