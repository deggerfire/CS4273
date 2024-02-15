library(shiny)  # The sever thingy
library(ggplot2)# Used for plotting
library(dplyr)  # Used for data handling

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
  titlePanel(img(src = "norman_pd_logo.jpeg", width = 150, height = 150)),
  
  # The blue bar on the top and dataset selector
  fluidRow(style = "background: #091682; color: white; padding: 14px;",
           # This is the drop down at the top.input$Data_Set will give you what the user has select from the list
           selectInput("Data_Set", "Data Set", list(`Graph Types` = c("Calls for Service", 
                                                                      "Collisions", 
                                                                      "Complaints, Inquiries and Use of force", 
                                                                      "Contacts", 
                                                                      "Offenses", 
                                                                      "Mixed"))),
  ),
  
  # Sidebar with demo selector parts
  sidebarLayout(
    sidebarPanel(
      # Sets the background color
      style = "background: #091682; color: white; padding: 20px",
      # A date range input
      dateRangeInput("dates", label = "Date range"),
      
      # Demo for the possible graph tools for ScatterPlot TODO: remove, here for ref
      conditionalPanel(condition = "input.graph == 'ScatterPlot'", 
                       radioButtons("radio", label = "Separator", choices = list("Flaming", "Eagle", "Acrobatic", "Raptor"))),
      width = 2
    ),
    
    # Show a plot of the generated distribution TODO: Make variable
    mainPanel(
      conditionalPanel(condition = "input.Data_Set == 'Calls for Service'", 
                       plotOutput("Barplot")
      ),
      conditionalPanel(condition = "input.Data_Set == 'Use of force'", 
                       plotOutput("Piechart")
      ),
      # demo code, TODO: remove/expand
      conditionalPanel(condition = "input.Data_Set == 'Mixed'",
                       column(width=9, plotOutput("Barplot2")),
                       column(width=3, plotOutput("Piechart2"))
      ),
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
  
  # Prints to the screen a barplot
    # data - the data that is to be rendered, must be tabled
    # legend - the title of the lengend
    # x/ylab - the titles for the x and y axis
  # exp: outputBarPlot(table(data$CallSource), legend = "Source of Call", xlab = "Source of Call", ylab = "Amount")
    # Makes a barplot based on CallSource (in the calls for service csv)
  outputBarPlot <- function(data, legend = "", xlab = "x", ylab = "y"){
    output$Barplot <- renderPlot({# Put the plot at plotOutput("Barplot") in the shiny code
      graph <- ggplot(data.frame(data), aes(x = Var1, y = Freq, fill = Var1)) # Setup graph data
      graph = graph + geom_bar(stat = "identity", width = .8)                 # Set up the data as a bar chart
      graph = graph + xlab(xlab) + ylab(ylab)                                 # Set the x/y labels
      graph = graph + guides(fill=guide_legend(title = legend))               # Set the title of the legend
      graph = graph + theme(text = element_text(size = 18))                   # Set the font size
      print(graph)                                                            # Print the graph
    })
  }
  
  # Prints to the screen a barplot
    # data - the data that is to be rendered, must be tabled
    # legend - the title of the lengend
  # exp: outputPieChart(data = table(data$RACE), legend = "Race")
    # Makes a piecahrt using Race (in the use of force csv)
  outputPieChart <- function(data, legend = ""){
    output$Piechart <- renderPlot({# Put the plot at plotOutput("Piechart") in the shiny code
      graph <- ggplot(data.frame(data), aes(x = "", y = Freq, fill = Var1))  # Set up graph data
      graph = graph + geom_bar(stat = "identity", width = 1)                 # Set up the data as a bar chart
      graph = graph + guides(fill=guide_legend(title = legend))              # Set the title of the legend
      graph = graph + theme_void() + theme(text = element_text(size = 18))   # Remove the background and set the font size
      graph = graph + coord_polar("y", start = 0)                            # Convert the graph to polar
      print(graph)                                                           # Print the graph
    })
  }
  
  # demo code, TODO: remove/expand
  outputMixed <- function(data1, data2, legend){
      output$Barplot2 <- renderPlot({
        graph <- ggplot(data1, aes(factor(CallSource), fill = CallSource))    # Setup graph data
        graph = graph + geom_bar(stat = "Count", position = position_dodge()) # Set up the data as a bar chart
        graph = graph + xlab("Source of Call") + ylab("Amount")               # Set the x/y labels
        graph = graph + guides(fill=guide_legend(title="Source of Call"))     # Set the title of the legend
        graph = graph + theme(text = element_text(size = 18))                 # Set the font size
        print(graph)                                                          # Print the graph
      })
      
      # Function to print out a pie chart using ggplot
        output$Piechart2 <- renderPlot({
          graph <- ggplot(data.frame(data2), aes(x = "", y = Freq, fill = Var1)) # Set up graph data
          graph = graph + geom_bar(stat = "identity", width = 1)                 # Set up the data as a bar chart
          graph = graph + guides(fill=guide_legend(title = legend))              # Set the title of the legend
          graph = graph + theme_void() + theme(text = element_text(size = 18))   # Remove the background and set the font size
          graph = graph + coord_polar("y", start = 0)                            # Convert the graph to polar
          print(graph)                                                           # Print the graph
        })
  }
  
  # Method that gets triggered when the graph is suppose to change
  observeEvent(input$Data_Set,{
    # Call both group A's and group L's trigger function
    groupAtrigger()
    groupLtrigger()
  })
  
  ######################################################################
  ########################  Group A Boundary ###########################
  ######################################################################
  # Groups A's method that gets triggered when the graph is suppose to change
  groupAtrigger <- function(){
    # If chain that checks for what type of graph is selected
      #(R's switch would not work here)
    if(input$Data_Set == "Use of force")
    {
      data <- read.csv(file("UOF.csv"))
      outputPieChart(data = table(data$RACE), legend = "Race")
    }
    else if(input$Data_Set == "Mixed")
    {
      data1 <- read.csv(file("CFS-2022.csv"))
      data2 <- read.csv(file("UOF.csv"))
      data2 <- table(data2$RACE)
      
      outputMixed(data1, data2, legend = "Race")
    }
  }
  ######################################################################
  ########################  Group L Boundary ###########################
  ######################################################################
  # Groups L's method that gets triggered when the graph is suppose to change
  groupLtrigger <- function(){
    # If chain that checks for what type of graph is selected
      #(R's switch would not work here)
    if(input$Data_Set == "Calls for Service")
    {
      data <- read.csv(file("CFS-2022.csv"))
      outputBarPlot(table(data$CallSource), legend = "Source of Call", xlab = "Source of Call", ylab = "Amount")
    }
  }
}

# This command runs our application- all you have to do to see the ouput is click
# the "Run App" button in the top right corner of RStudio.

# A webpage will open and allow you to interact with it. 
shinyApp(ui = ui, server = server)