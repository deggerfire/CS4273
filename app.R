library(shiny)          # The sever thingy
library(ggplot2)        # Used for plotting
library(dplyr)          # Used for data handling
library(shinydashboard) # Used for fancy UI stuff

# Define UI for the application

# this is where we need to define different pages/tabs for us, and then 
# contribute separately. potentially use the navbarPage function to create

# Note- the ENTIRE UI is contained in this page. to prevent merge issues, I have
# seperated out two sections of code for Group A and Group L.
source("tabs/CFStab.R")
source("tabs/COLtab.R")
source("tabs/UOFtab.R")
source("tabs/CONtab.R")
source("tabs/OFFtab.R")
ui <- dashboardPage(
  ######################################################################
  ########################  Universal Boundary #########################
  ######################################################################
  
  # Sets the title
  dashboardHeader(title='Norman PD', titleWidth = 295),
  
  # Left sidebar, used to to get to major catogories
  dashboardSidebar(
    width = 295,
    sidebarMenu(
      # Variable name of this sidebar
      id = "sidebar",
      #             name on the sidebar for user                  var name in code      icon on screen
      menuItem("Calls for Service"                               , tabName = "CFS", icon = icon("phone")),
      menuItem("Collisions"                                      , tabName = "COL", icon = icon("car-burst"),
        menuSubItem('By Severity'                                , tabName = 'COLl', icon = icon('triangle-exclamation')),
        menuSubItem('By injury'                                  , tabName = 'COL2', icon = icon('user-injured'))),
      menuItem("Complaints, Inquiries and Use of force"          , tabName = "UOF", icon = icon("gun")),
        #menuSubItem('Incidents by Type and Disposition'          , tabName = 'UOF1'),
        #menuSubItem('Subjects by Incidents and Demographics'     , tabName = 'UOF2'),
        #menuSubItem('Subjects by Allegation and Finding'         , tabName = 'UOF3'),
        #menuSubItem('Subjects by Resistance and Force'           , tabName = 'UOF4')),
      menuItem("Contacts"                                        , tabName = "CON", icon = icon("hand")),
        #menuSubItem('Traffic and Parking Contacts'               , tabName = 'CON1')),
      menuItem("Offenses"                                        , tabName = "OFF", icon = icon("handcuffs"))
        #menuSubItem('Case Offenses'                              , tabName = 'OFF1',),
        #menuSubItem('Case Details'                               , tabName = 'OFF2',),
        #menuSubItem('Subjects'                                   , tabName = 'OFF3',),
        #menuSubItem('Arrests'                                    , tabName = 'OFF4',))
        
    )
  ),
  
  # Main body where graphs are rendered (they are all in their own files)
  dashboardBody(id = "tabs",
    tabItems(
      CFS_tab(), # Calls for service tab
      COL_tab(), # Collision Tab
      UOF_tab(), # Use of force Tab
      CON_tab(), # Contacts Tab
      OFF_tab()  # Offense Tab
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
  # Prints to the screen a barplot
    # data - the data that is to be rendered, must be tabled
    # legend - the title of the lengend
    # x/ylab - the titles for the x and y axis
  # exp: outputBarPlot(table(data$CallSource), legend = "Source of Call", xlab = "Source of Call", ylab = "Amount")
    # Makes a barplot based on CallSource (in the calls for service csv)
  outputBarPlot <- function(data, label = ""){
    plot <- renderPlot({# Put the plot at plotOutput("Barplot") in the shiny code
      graph <- ggplot(data.frame(data), aes(x = Var1, y = Freq, fill = Var1)) # Setup graph data
      graph = graph + geom_bar(stat = "identity", width = .8)                 # Set up the data as a bar chart
      graph = graph + xlab(label) + ylab("Amount")                                # Set the x/y labels
      graph = graph + guides(fill=guide_legend(title = label))                # Set the title of the legend
      graph = graph + theme(text = element_text(size = 18))                   # Set the font size
      print(graph)                                                            # Print the graph
      }
    )
    return(plot)
  }
  
  # Prints to the screen a barplot
    # data - the data that is to be rendered, must be tabled
    # legend - the title of the lengend
  # exp: outputPieChart(data = table(data$RACE), legend = "Race")
    # Makes a piecahrt using Race (in the use of force csv)
  outputPieChart <- function(data, label = ""){
    plot <- renderPlot({# Put the plot at plotOutput("Piechart") in the shiny code
      graph <- ggplot(data.frame(data), aes(x = "", y = Freq, fill = Var1))  # Set up graph data
      graph = graph + geom_bar(stat = "identity", width = 1)                 # Set up the data as a bar chart
      graph = graph + guides(fill=guide_legend(title = label))               # Set the title of the legend
      graph = graph + theme_void() + theme(text = element_text(size = 18))   # Remove the background and set the font size
      graph = graph + coord_polar("y", start = 0)                            # Convert the graph to polar
      print(graph)                                                           # Print the graph
    })
    return(plot)
  }

  # Method that gets triggered when the graph is suppose to change
  observeEvent(input$sidebar, {
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
    if(input$sidebar == "UOF")
    {
      data <- read.csv(file("UOF.csv"))
      race <- outputPieChart(table(data$RACE), label = "Race")
      sex <- outputPieChart(table(data$SEX), label = "Sex")
      
      involvement <- outputBarPlot(table(data$RACE), label = "Involvement")
      subject_type <- outputBarPlot(table(data$SEX), label = "Subject_Type")
      
      UOF_render(output, race, sex, involvement, subject_type)
      
      output$UOF_table_1 <- race
      output$UOF_table_2 <- sex
      output$UOF_table_3 <- involvement
      output$UOF_table_4 <- subject_type
    }
  }
  ######################################################################
  ########################  Group L Boundary ###########################
  ######################################################################
  # Groups L's method that gets triggered when the graph is suppose to change
  groupLtrigger <- function(){
    # If chain that checks for what type of graph is selected
      #(R's switch would not work here)
    if(input$sidebar == "CFS")
    {
      
      
      # Read in call for service data (this is temp)
      data <- read.csv(file("CFS-2022.csv"))# read in data
      
      
      #test45 <- data[data$IncidentType == "Traffic Stop",]
      #head(test45)
      ## dylpr data 2
      demo <- outputBarPlot (table(data$CallSource        ), label = "Source of Call")
      
      
      
      # Render the data then send it to be put on screen, for now you have to send all graphs
      CFS_render(output,
                 demo,
                 outputPieChart(table(data$PoliceCallStatus  ), label = "PoliceCallStatus"),
                 outputBarPlot (table(data$PoliceCallPriority), label = "PoliceCallPriority"),
                 outputPieChart(table(data$City              ), label = "City"),
                 outputPieChart(table(data$Zip               ), label = "Zip"))
      ##output$CFS_table_1 <- demo
      #output$CFS_table_2 <- demo
      #output$CFS_table_3 <- demo
      
      output$CON_table_1 <- demo
      output$OFF_table_1 <- demo
      
    }
    # If block for call for service tab
    
  }
}

# This command runs our application- all you have to do to see the ouput is click
# the "Run App" button in the top right corner of RStudio.

# A webpage will open and allow you to interact with it. 
shinyApp(ui = ui, server = server)