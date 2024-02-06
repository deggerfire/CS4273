library(shiny)  # The sever thingy
library(ggplot2)# Used for plotting
library(dplyr)  # Used for data handling

# Demo data just for testing TODO: Remove
# ddt <- read.csv(file("CFS-2022.csv"))
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
      # A date range input
      dateRangeInput("dates", label = "Date range"),
      # Get type of graph the user wants
      selectInput("graph", "Graph Type", list(`Graph Types` = c("ScatterPlot", "Histogram", "Boxplot"))),
      # Set what the user wants on the Y-axis
      selectInput("yAxis", "Y axis", list(`options` = c(sort(colnames(ddt))))),
      # Set what the user wants on the X-axis
      selectInput("xAxis", "X axis",list(`options` = c(sort(colnames(ddt))))),
      
      # Demo for the possible graph tools for ScatterPlot
      conditionalPanel(condition = "input.graph == 'ScatterPlot'", 
                       radioButtons("radio", label = "Separator", choices = list("Flaming", "Eagle", "Acrobatic", "Raptor"))),
      
      # Demo for the possible graph tools for histogram
      conditionalPanel(condition = "input.graph == 'Histogram'", 
                       sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)),
      
      # Demo for the possible graph tools for Boxplot
      conditionalPanel(condition = "input.graph == 'Boxplot'", 
                       checkboxGroupInput("checkGroup", label = h3("Checkbox group"), choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), selected = 1))
      ),
    
    # Show a plot of the generated distribution TODO: Make variable
    mainPanel(
      conditionalPanel(condition = "input.graph == 'ScatterPlot'", 
                        plotOutput("ScatterPlot")
      ),
      conditionalPanel(condition = "input.graph == 'Histogram'", 
                       plotOutput("Histogram")
      ),
      conditionalPanel(condition = "input.graph == 'Boxplot'", 
                       plotOutput("Boxplot")
      )
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
  output$ScatterPlot <- renderPlot({
    g = ggplot(ddt, aes(x = ddt[,input$xAxis], y = ddt[,input$yAxis], colour = ddt[,input$yAxis]))
    g + ggtitle("demo graph")
    g = g + geom_point()
    print(g)
  })
  
  output$Histogram <- renderPlot({
    # This is a R code area-------------------------------------------------------------------------------------------
    # To get this area to render in the graph type drop down select histogram
    
    # Data area ##############################################################################
    # This is where you can mess to data before trying to render it
    
    # Make random data to put into a histogram
    set.seed(123)
    df <- data.frame(
      gender=factor(rep(c(
        "Average Female income ", "Average Male incmome"), each=20000)),
      Average_income=round(c(rnorm(20000, mean=15500, sd=500), 
                             rnorm(20000, mean=17500, sd=600)))   
    )
    ##########################################################################################
    
    # Graph area #############################################################################
    # This is where you graph things !use ggplot!
    # Make g that uses the df variable from above for this graph
    g <- ggplot(df, aes(x=Average_income))
    # Set g to be a histogram
    g = g + geom_histogram()
    # Print g
    print(g)
    ##########################################################################################
  })
  
  output$Boxplot <- renderPlot({
    ToothGrowth$dose <- as.factor(ToothGrowth$dose)
    g <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
      geom_boxplot()
    print(g)
    # This is a R code area-------------------------------------------------------------------------------------------
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
