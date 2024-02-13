library(shiny)  # The sever thingy
library(ggplot2)# Used for plotting
library(dplyr)  # Used for data handling
library(shinydashboard) # guess what it's used for

# Define UI for the application

# this is where we need to define different pages/tabs for us, and then 
# contribute separately. potentially use the navbarPage function to create

# Note- the ENTIRE UI is contained in this page. to prevent merge issues, I have
# seperated out two sections of code for Group A and Group L.

ui <- dashboardPage(
  ######################################################################
  ########################  Universal Boundary #########################
  ######################################################################
  
  # Sets the title on top left
  dashboardHeader(title='Norman PD'),
  
  # This sets the tabs on the left, and allows them to open/close.
  dashboardSidebar(
    sidebarMenu(
      
      # Each of these menu items are tabs, and can be clicked. tabName is used later in dashboardBody
      menuItem("Use of Force", tabName = "UOF", icon = icon("dashboard")),
      menuItem("Calls for Service", tabName = "CFS", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # This is the first tab, and is the default tab that opens
      tabItem(tabName = "UOF",
              fluidRow(
                box(width = 6, height=300, plotOutput("Piechart")),
                box( width = 6, height=300, plotOutput("Barplot")),
                box( width = 6, height=300, plotOutput("Piechart")),
                box( width = 6, height=300, plotOutput("Barplot")),
            )
      ),
      # This is the second tab, and is the default tab that opens
      tabItem(tabName = "CFS",
              fluidRow(
                tabBox(
                  title = "First tabBox",
                  id = "Barplot", height = "250px",
                  tabPanel("Tab1", "First tab content"),
                  tabPanel("Tab2", "Tab content 2")
                ),
                tabBox(
                  title = "Second tabBox",
                  id = "Piechart", height = "250px",
                  tabPanel("Tab1", "First tab content"),
                  tabPanel("Tab2", "Tab content 2")
                ),
              )
      )
    )
  ),
  
  
  
  ######################################################################
  ########################  Group A Boundary ###########################
  ######################################################################
  
  ######################################################################
  ########################  Group L Boundary ###########################
  ######################################################################
)

# This is where we will define server logic. Ie, this is where we will parse the CSV,
# add graphs, create sliders/filters for user input, ect

# The bulk of our work will be here. Again, I have sectioned off the code for 
# Group A and Group L to prevent merge issues.

server <- function(input, output) {
  ######################################################################
  ########################  Universal Boundary #########################
  ######################################################################
  # Display a scatter plot with the data
  outputBarPlot <- function(){
    output$Barplot <- renderPlot({
      graph <- ggplot(data, aes(factor(CallSource), fill = CallSource))     # Setup graph data
      graph = graph + geom_bar(stat = "Count", position = position_dodge()) # Set up the data as a bar chart
      graph = graph + xlab("Source of Call") + ylab("Amount")               # Set the x/y labels
      graph = graph + guides(fill=guide_legend(title="Source of Call"))     # Set the title of the legend
      graph = graph + theme(text = element_text(size = 18))                 # Set the font size
      print(graph)                                                          # Print the graph
    })
  }
  
  # Function to print out a pie chart using ggplot
  outputPieChart <- function(data, legend){
    output$Piechart <- renderPlot({
      graph <- ggplot(data.frame(data), aes(x = "", y = Freq, fill = Var1))  # Set up graph data
      graph = graph + geom_bar(stat = "identity", width = 1)                 # Set up the data as a bar chart
      graph = graph + guides(fill=guide_legend(title = legend))              # Set the title of the legend
      graph = graph + theme_void() + theme(text = element_text(size = 18))   # Remove the background and set the font size
      graph = graph + coord_polar("y", start = 0)                            # Convert the graph to polar
      print(graph)                                                           # Print the graph
    })
  }
  
  # Data Selector TODO: make work
  observeEvent(input$Data_Set,{
    # Remove if chain at some point
    if(input$Data_Set == "Calls for Service")
    {
      data <- read.csv(file("CFS-2022.csv"))
      outputBarPlot()
    }
    else if(input$Data_Set == "Use of force")
    {
      data <- read.csv(file("UOF.csv"))
      outputPieChart(data = table(data$RACE), legend = "Race")
    }
    else{print("Error, invalid selection")}
    
    
    groupAeffect()
    groupLeffect()
  })
  
  ######################################################################
  ########################  Group A Boundary ###########################
  ######################################################################
  groupAeffect <- function(){
    # At some point group A's if chain will be here
  }
  ######################################################################
  ########################  Group L Boundary ###########################
  ######################################################################
  groupLeffect <- function(){
    # At some point group L's if chain will be here
  }
}

# This command runs our application- all you have to do to see the ouput is click
# the "Run App" button in the top right corner of RStudio.

# A webpage will open and allow you to interact with it. 
shinyApp(ui = ui, server = server)
