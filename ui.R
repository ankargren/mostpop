#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  title = "Popularity of albums",
  # Application title
  titlePanel("Popularity of albums"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    
  column(6,
         wellPanel(
           textInput("search_name", label = h3("Name of artist"), value = "Elliott Smith", width = NULL, placeholder = NULL), 
           helpText("Type in the name of an artist to see which of its albums are more popular."),
           actionButton("go", "Search")
         )
    ),
  column(4,
         uiOutput("player")
  )
  ),
  plotlyOutput("Plot")
))
