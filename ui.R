library(shiny)
library(ggplot2)

dataset <- diamonds

fluidPage(

  titlePanel("Data Explorer"),

  
  sidebarPanel(
    uiOutput("featureControls")
  ),
  
  mainPanel(
    # textOutput('randomForestExp'),
    plotOutput('plot')
    
  )
)