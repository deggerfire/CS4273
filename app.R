library(shiny)

# Define UI for the application

# this is where we need to define different pages/tabs for us, and then 
 # contribute separately. potentially use the navbarPage function to create

# Note- the ENTIRE UI is contained in this page. to prevent merge issues, I have
  # seperated out two sections of code for Group A and Group L.

ui <- fluidPage(
  
  ######################################################################
  ########################  Group A Boundary ###########################
  ######################################################################

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
  
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
  ########################  Group A Boundary ###########################
  ######################################################################

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    
    
    
    
    
    
    
    
    ######################################################################
    ########################  Group L Boundary ###########################
    ######################################################################
}

# This command runs our application- all you have to do to see the ouput is click
  # the "Run App" button in the top right corner of RStudio.

# A webpage will open and allow you to interact with it. 
shinyApp(ui = ui, server = server)
