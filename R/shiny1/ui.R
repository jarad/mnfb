
library(shiny)
load('birdtot.Rdata')
  
shinyUI(bootstrapPage(
  headerPanel("Bird Count Trend"), 
  
  sidebarPanel( 
    selectInput(inputId = "specie",
                label = "Select Bird specie:",
                choices = levels( bird.tot$abbrev )
    )

  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Average", plotOutput("plot1")),
      tabPanel("Total", plotOutput("plot2")),
      tabPanel("Summary", tableOutput("summary"))
      
    )
  )
  
))
