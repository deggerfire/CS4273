library(shiny)  # The sever thingy
library(ggplot2)# Used for plotting

# Demo data just for testing TODO: Remove
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
  titlePanel(img(src="logo.png")),
  
  # Sidebar with demo selector parts
  sidebarLayout(
    sidebarPanel(
      # Get type of graph the user wants
      selectInput("graph", "Graph Type", list(`Graph Types` = c("ScatterPlot", "Histogram", "Boxplot"))),
      # Set what the user wants on the Y-axis
      selectInput("yAxis", "Y axis", list(`options` = c(sort(colnames(ddt))))),
      # Set what the user wants on the X-axis
      selectInput("xAxis", "X axis",list(`options` = c(sort(colnames(ddt))))),
      # Demo for the possible graph tools
      sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
    ),
    
    # Show a plot of the generated distribution TODO: Make variable
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
  
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
  output$scatterPlot <- renderPlot({
    g = ggplot(ddt, aes(x = ddt[,input$xAxis], y = ddt[,input$yAxis], colour = ddt[,input$yAxis]))
    g + ggtitle("demo graph")
    g = g + geom_point()
    print(g)
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
