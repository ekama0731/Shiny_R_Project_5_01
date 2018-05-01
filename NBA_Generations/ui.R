library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("NBA Generation Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("generation",
                   "Select a generation:",
                   min = 2,
                   max = 16,
                   value = 2)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("genGraph")
    )
  )
))
