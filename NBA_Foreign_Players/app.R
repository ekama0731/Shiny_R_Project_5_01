library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(rsconnect)

   ui <- dashboardPage(skin = "black",
                      
  header <- dashboardHeader(title = "NBA Birth Place Analysis", titleWidth = 250),
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("International Birth Place", tabName = "Foreign", icon = icon("globe")),
      menuItem("NBA Landscape Trend", tabName = "Analysis", icon = icon("adjust")),
      menuItem('U.S. Birth Place', tabName = 'US', icon = icon('hourglass')),
      menuItem('NBA Data Set', tabName = 'BirthData', icon = icon('search'))
      )),
      
  
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = 'Foreign', 
              fluidRow( 
                box(title = 'Foreign Born', solidHeader=TRUE, status = 'danger', width = 12, 
                    plotOutput('top20')))),
      tabItem(tabName = 'Analysis',
              fluidRow(
                title = 'NBA Macro Scope', solidHeader = TRUE, status = 'info', 
                    plotlyOutput("trend"))),
      
      tabItem(tabName = 'US',
              fluidRow(
                box(title = 'US Birth Place', solidHeader = TRUE, Status = 'warning', width = 12, 
                    plotOutput("genGraph")),
                box(title = "Generation Controls", solidHeader = TRUE, width = 12,
                    sliderInput("generation", "Generation Number:", 2, 16, 2)))
              ),
      
      tabItem(tabName = 'BirthData',
              fluidPage(titlePanel("Birth Place Data")),
              fluidRow(
              column(width = 4,
                     selectInput("birth_state",
                                 "Birth Place:",
                                 c("All",
                                   unique(as.character(foreignState$birth_state))))),
              column(4,
                     selectInput("born",
                                 "Year Born:",
                                 c("All",
                                   unique(as.character(foreignState$born)))))
              ),
              fluidRow(
                box(width = 12,
                DT::dataTableOutput("foreignState")))
                
              ) # close fluid row
              )#close tab item
      
      
      ) #closing tab items 
  )#closing body 
  
 #closing ui 
  
  
  
# Define server logic required to draw a graph
server <- function(input, output) {
  
  
  output$top20 <- renderPlot({
    
    return(
      ggplot(data=foreignStateTop10, aes(x=reorder(birth_state,country_count), y=country_count)) +
        geom_bar(stat = 'identity',fill="#FF9999", colour="black") +
        geom_text(aes(label =foreignStateTop10$country_count, vjust=-.25))+
        theme(plot.subtitle = element_text(size = 15, hjust = 0.5, vjust = 1), 
              plot.caption = element_text(vjust = 1), 
              axis.line = element_line(size = 0.4, linetype = "solid"), 
              axis.ticks = element_line(colour = "black", size = 1), 
              panel.grid.major = element_line(colour = NA), 
              panel.grid.minor = element_line(colour = NA), 
              axis.title = element_text(family = "mono", face = "bold"),
              axis.text = element_text(family = "mono", face = "bold", colour = "black"), 
              axis.text.x = element_text(family = "mono", colour = "black", vjust = 0.5, angle = 45, size = 15), 
              axis.text.y = element_text(colour = "black"), 
              plot.title = element_text(family = "mono", size = 20, face = "bold", hjust = 0.5), 
              panel.background = element_rect(fill = NA)) +
        labs(title = "International NBA Player Birth Places", x = "Birth Places", y = "Total Number ", colour = "Green", subtitle = "Top 15 locations")
      
      
    )# close return 
  }) #closing renderplot patenthese
  
  
  output$trend <- renderPlotly({
    return(
      plot_ly(data = percentChange, type = 'scatter', x= ~born, y= ~perChange, color= ~us, colors = "Set1", hoverinfo = 'text', 
              text = ~paste('Percent Difference: ', perChange,
                            '<br> Location: ', us )) %>%
        layout(title = 'Trend Bewtween US vs World NBA PLayers',titlefont = 'mono',xaxis=b, yaxis=c,autosize = F, width = 750, height = 750, margin = m)
      
    )
      
      
  s})#closing renderplot parentheses
  
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
        labs(title = "NBA Player Birth Places per Generation", x = "States", y = "Number of Players", fill = "Number of Players"))
  }) # end of genGraph render plot
  
  output$foreignState <- DT::renderDataTable(
    DT:: datatable({
    if (input$birth_state != "All") {
      foreignState <- foreignState[foreignState$birth_state == input$birth_state,]
    }
    if (input$born != "All") {
      foreignState <- foreignState[foreignState$born == input$born,]
    }
    foreignState
  }))
  
  
} #closing server bracket
# Run the application 
shinyApp(ui = ui, server = server)


