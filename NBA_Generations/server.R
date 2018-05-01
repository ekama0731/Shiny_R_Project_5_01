library(shiny)
library(RColorBrewer)
library(shinydashboard)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$genGraph <- renderPlot({
    
    # generate dataset based on input$generation from ui.R
    gen_graph <- nba_gen_season %>% filter(.,  generation == input$generation)
    
    # draw the bar chart with the specified generation 
    return(
      ggplot(data=gen_graph, aes(x=reorder(birth_state,count), y= count)) +
        geom_bar(stat = 'identity', aes(fill=count)) + 
        geom_text(aes(label =gen_graph$count, vjust=-.25)) +
        scale_fill_gradient(low="white", high="red") +
        theme(plot.subtitle = element_text(size = 10, colour = "black", hjust = 0.5, vjust = 1), 
              plot.caption = element_text(vjust = 1), 
              axis.line = element_line(size = 0.4, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size = 0.6), 
              panel.grid.major = element_line(colour = NA), 
              axis.title = element_text(face = "bold"), 
              axis.text = element_text(face = "bold", colour = "black"), 
              axis.text.x = element_text(colour = "black", vjust = 0.5, angle = 90), 
              axis.text.y = element_text(colour = "black"), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
              panel.background = element_rect(fill = NA), 
              legend.background = element_rect(colour = "aliceblue")) +
        labs(title = "Top 20 NBA Player Birth States", x = "States", y = "Number of Players", fill = "Number of Players", subtitle = "NBA players birth states from 1915-1997")
        
        
    )
    
    # generate dataset based on nba season top 20 from ui.R
    
    
    # draw the bar chart with the secified generation
      
  })
  
})
