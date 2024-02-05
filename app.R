library(shiny)
library(ggplot2)

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
    titlePanel(img(src="logo.png")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          div(
            selectInput("yAxis", "Y axis", list(`options` = c(sort(colnames(ddt))))),
            " VS ",
            selectInput("xAxis", "X axis",list(`options` = c(sort(colnames(ddt))))),
          )
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
      g = ggplot(ddt, aes(x = ddt[,input$xAxis], y = ddt[,input$yAxis], colour = ddt[,input$yAxis]))
      g + ggtitle("demo graph")
      g = g + geom_point()
      print(g)
    })
    
    
    
    
    
    
    
    
    
    ######################################################################
    ########################  Group L Boundary ###########################
    ######################################################################
}

# This command runs our application- all you have to do to see the ouput is click
  # the "Run App" button in the top right corner of RStudio.

# A webpage will open and allow you to interact with it. 
shinyApp(ui = ui, server = server)
