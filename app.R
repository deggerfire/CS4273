library(shiny)  # The sever thingy
library(ggplot2)# Used for plotting
library(dplyr)  # Used for data handling

# Demo data just for testing TODO: Remove
data <- read.csv(file("CFS-2022.csv"))
ddt <- read.csv(file("DDT.csv"))

# Define UI for the application

# this is where we need to define different pages/tabs for us, and then 
 # contribute separately. potentially use the navbarPage function to create

# Note- the ENTIRE UI is contained in this page. to prevent merge issues, I have
  # seperated out two sections of code for Group A and Group L.

ui <- fluidPage(
  ######################################################################
  ########################  Universal Boundary #########################
  ######################################################################
  
  # Put in the top bar, just a demo image for now
  titlePanel(img(src="logoV2.png")),
  
  # Sidebar with demo selector parts
  sidebarLayout(
    sidebarPanel(
      # Sets the background color
      style = "background: #4c5cad; color: white",
      # Get type of graph the user wants
      selectInput("Data_Set", "Data Set", list(`Graph Types` = c("Calls for Service", "Collisions", "Complaints, etc"))),
      # A date range input
      dateRangeInput("dates", label = "Date range"),
      
      # Demo for the possible graph tools for ScatterPlot
      conditionalPanel(condition = "input.graph == 'ScatterPlot'", 
                       radioButtons("radio", label = "Separator", choices = list("Flaming", "Eagle", "Acrobatic", "Raptor"))),
      
      # Demo for the possible graph tools for histogram
      conditionalPanel(condition = "input.graph == 'Histogram'", 
                       sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)),
      
      # Demo for the possible graph tools for Boxplot
      conditionalPanel(condition = "input.graph == 'Boxplot'", 
                       checkboxGroupInput("checkGroup", label = h3("Checkbox group"), choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), selected = 1)),
      width = 2
      ),
    
    # Show a plot of the generated distribution TODO: Make variable
    mainPanel(
      plotOutput("Barplot")
    )
  ),style='margins: -21px'
  
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
  output$Barplot <- renderPlot({
    graph <- ggplot(data, aes(factor(CallSource), fill = CallSource))
    graph = graph + geom_bar(stat = "Count", position = position_dodge())
    graph = graph + theme(text = element_text(size = 18))
    print(graph)
  })
  
  # Data Selector
  observeEvent(input$Data_Set,{
    switch(
      input$Data_Set,
      "Calls for Service" = 
        data <- read.csv(file("CFS-2022.csv")),
      "Collisions" = print("test2"),
      "Complaints, etc" = print("test3")
    )
    
  })
  
  ######################################################################
  ########################  Group A Boundary ###########################
  ######################################################################

  ######################################################################
  ########################  Group L Boundary ###########################
  ######################################################################
}

# This command runs our application- all you have to do to see the ouput is click
  # the "Run App" button in the top right corner of RStudio.

# A webpage will open and allow you to interact with it. 
shinyApp(ui = ui, server = server)
