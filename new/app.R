library(shinydashboard)

ui <- dashboardPage(skin = "black",
  header <- dashboardHeader(title = "NBA Birth Place Analysis", titleWidth = 250),
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Generational Birth Place", tabName = "Analysis", icon = icon("globe")),
      menuItem("Widgets", tabName = "widgets", icon = icon("home")))),
  
  body <- dashboardBody(
    tabItems(
#first tab content
      tabItem(tabName = 'Analysis',
    fluidRow(
      box(title = 'NBA Birth Place', solidHeader = TRUE, status = 'warning', width=12,
          plotOutput("genGraph")),
      box(title = "Generation Controls", solidHeader = TRUE, width = 12,
          sliderInput("generation", "Generation Number:", 2, 16, 2)))),
#second tab content
    tabItem(tabName = 'widgets', 
  fluidRow( 
    box(title = 'U.S. or Foreign Born', solidHeader=TRUE, status = 'warning', width = 12,
        plotOutput('outsideVsInside')),
    box(title = 'Generation Controls', solidHeader=TRUE, 
        sliderInput('generation', 'Gerneration number:',2,16,2)))


)) # end of the body statement

))#end of body parentheses


server <- function(input, output) {
  # genGraph plot 
  output$genGraph <- renderPlot({
    # generate dataset based on input$generation from ui.R
    gen_graph <- nba_gen_season %>% filter(generation == input$generation)
    # draw the bar chart with the specified generation 
    return(
      ggplot(data=gen_graph, aes(x=reorder(birth_state,count), y= count)) +
        geom_bar(stat = 'identity', aes(fill=count)) + 
        geom_text(aes(label =gen_graph$count, vjust=-.25)) +
        scale_fill_gradient(low="Papaya Whip", high="Orange") +
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
        labs(title = "NBA Player Birth Places per Generation", x = "States", y = "Number of Players", fill = "Number of Players", subtitle = "NBA players birth states from "))
  }) # end of genGraph render plot

  output$bornPlaces <- renderPlot({
    # generating a dataset based on input$generation
    bornPlaces = inUs %>% filter(., inUs$generation == input$generation)
    # plot to see generation by generation of players between US vs Outside
    
    bornPlaces = ggplot(data=bornPlaces, aes(x=in_us))+ geom_bar()
    
  }) # end of outsideGraph render plot
  
  
} # the end to our server 

shinyApp(ui, server)

