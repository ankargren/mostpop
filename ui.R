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
  
  # Application title
  titlePanel("Spotify App"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    
  column(3,
         wellPanel(
           textInput("clientID", label = h4("Client ID"), value = "a8ff287d905549a19a2a6463d48e44a2", width = NULL, placeholder = NULL),
           textInput("secret", label = h4("Secret"), value = "3a54a7a6f0ea4b2db4539c61b2a892da", width = NULL, placeholder = NULL)
         ),
    uiOutput("player")
  ),
  
  column(9,
         wellPanel(
           textInput("search_name", label = h3("Name of artist"), value = "Kent", width = NULL, placeholder = NULL), 
           helpText("Type in the name of an artist to see which of its albums are more popular.")
         ),
      plotOutput("Plot")
    )
  )
))
