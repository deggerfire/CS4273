library(shiny)          # The sever thingy
library(ggplot2)        # Used for plotting
library(dplyr)          # Used for data handling
library(shinydashboard) # Used for fancy UI stuff

# Stuff for running a server
#options(shiny.host = '10.204.155.94') # IP-address of computer
#options(shiny.port = 5111) # Port you want to host on

# Import the tab files
source("tabs/CFStab.R")
source("tabs/COLtab.R")
source("tabs/UOFtab.R")
source("tabs/CONtab.R")
source("tabs/OFFtab.R")
ui <- dashboardPage(
  ######################################################################
  ######################################################################
  #######                      Main page UI                     ########
  ####### UI people should be the only ones making changes here ########
  ######################################################################
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
)

# This is where we will define server logic. Ie, this is where we will parse the CSV,
# add graphs, create sliders/filters for user input, ect

# The bulk of our work will be here. Most of the time you will be 
# working in your teams trigger method

server <- function(input, output, session) {
  ######################################################################
  ######################################################################
  #######################  Graph making methods ########################
  ######################################################################
  ######################################################################
  # Makes a barplot object using the inputted data
  # This function is for step 3
    # data - the data that is to be rendered, must be tabled
    # label - string for graph labels
  outputBarPlot <- function(data, label = ""){
    plot <- renderPlot({# Put the plot at plotOutput("Barplot") in the shiny code
      graph <- ggplot(data.frame(data), aes(x = Var1, y = Freq, fill = Var1)) # Setup graph data
      graph = graph + geom_bar(stat = "identity", width = .8)                 # Set up the data as a bar chart
      graph = graph + xlab(label) + ylab("Amount")                                # Set the x/y labels
      graph = graph + guides(fill=guide_legend(title = label))                # Set the title of the legend
      graph = graph + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1))                   # Set the font size
      print(graph)                                                            # Print the graph
      }
    )
    return(plot)
  }
  
  # Makes a piechart object using the inputted data
  # This function is for step 3
    # data - the data that is to be rendered, must be tabled
    # label - string for graph labels
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
  
  ######################################################################
  ######################################################################
  ######## observeEvent/trigger (react to user doing something) ########
  ######################################################################
  ######################################################################
  
  # List that triggers observeEvent() whenever anything on the screen is changed
  toObserve <- reactive({
    reactiveValuesToList(input)
  })
  
  # Method that gets triggered when the graph is suppose to change or update
  observeEvent(toObserve(), {
    # Call both group A's and group L's trigger function
    groupAtrigger()
    groupLtrigger()
  })
  
  ######################################################################
  ###################  Group A's trigger method ########################
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
  ###################  Group L's trigger method ########################
  ######################################################################
  # Groups L's method that gets triggered when the graph is suppose to change
  groupLtrigger <- function(){
    # If chain that checks for what type of graph is selected
      #(R's switch would not work here)
    if(input$sidebar == "CFS")
    {
      ######################
      # Step 1: read in the data
      ######################
      # Read in the call for service 2022
      data <- read.csv(file("CFS-2022.csv"))
      # Populate the widgets in CFS
      CFS_populate_Widgets(session, data$CallSource, data$PoliceCallStatus, data$PoliceCallPriority, data$City, data$PoliceCallType)
      ######################
      # Step 2: Filter the data
      ######################
      
      # If the user has selected an input for source of call then remove all that does not have the selected input
      if(input$CFS_Source_of_Call_Selector != "Unselected"){
        data <- data %>% filter(CallSource == input$CFS_Source_of_Call_Selector)
      }
      
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      # Makes the graph for source of call
      CS_BP   <- outputBarPlot (table(data$CallSource        ), label = "Source of Call")
      # Makes the graph for police call status
      PCS_PC  <- outputPieChart(table(data$PoliceCallStatus  ), label = "PoliceCallStatus")
      # Makes the graph for police call priority
      PCP_BP  <- outputBarPlot (table(data$PoliceCallPriority), label = "PoliceCallPriority")
      # Makes the graph for city
      City_PC <- outputPieChart(table(data$City              ), label = "City")
      
      ######################
      # Step 4: Put the graphs on screen
      ######################
      # Send the graphs off to the call for service render function to be put on screen
      CFS_render(output, CS_BP, PCS_PC, PCP_BP, City_PC)
      
    }
    else if(input$sidebar == "CON"){
      ######################
      # Step 1: read in the data
      ######################
      data <- read.csv(file("Contacts.csv"))
      CON_populate_Widgets(session, data$Sex, data$Race, data$Race, data$Race, data$Race)
      ######################
      # Step 2: Format the data
      ######################
      if(input$CON_Selector_1 != "Unselected"){
        data <- data %>% filter(Sex == input$CON_Selector_1)
      }
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      Contacts_Sex   <- outputPieChart (table(data$Sex), label = "Sex")
      Contacts_Race   <- outputBarPlot (table(data$Race), label = "Race")
      ######################
      # Step 4: Put the graphs on screen
      ######################
      CON_render(output, Contacts_Sex, Contacts_Race, Contacts_Race, Contacts_Race)
    }
    else if(input$sidebar == "OFF"){
      ######################
      # Step 1: read in the data
      ######################
      ######################
      # Step 2: Format the data
      ######################
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      ######################
      # Step 4: Put the graphs on screen
      ######################
    }
    # If block for call for service tab
    
  }
}

# This command runs our application- all you have to do to see the ouput is click
# the "Run App" button in the top right corner of RStudio.

# A webpage will open and allow you to interact with it. 
shinyApp(ui = ui, server = server)