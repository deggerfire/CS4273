##################################################################
##################################################################
##              File for the call for Contacts                  ##
##################################################################
##################################################################

# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
CON_selectors <- c("CON_Selector_1"    , 
                   "CON_Selector_2", 
                   "CON_Selector_3", 
                   "CON_Selector_4"
             )

# Render function for call for service (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
CON_render <- function(output, plot1, plot2, plot3, plot4){
  output$CON_table_1 <- plot1
  output$CON_table_2 <- plot2
  output$CON_table_3 <- plot3
  output$CON_table_4 <- plot4
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
CON_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "CON",
    # Makes the first graph area
    tabBox(
      height = "500px",
      # Uses functions to make what is in each tab (string is the name of the plotOutput)
      CON_Plot("TAB 1", "CON_table_1", CON_selectors[1]),
      CON_Plot("TAB 2", "CON_table_2", CON_selectors[2])
    ),
    # Makes the second graph area
    tabBox(
      height = "500px",
      # Uses functions to make what is in each tab (string is the name of the plotOutput)
      CON_Plot("TAB 3", "CON_table_3", CON_selectors[3]),
      CON_Plot("TAB 4", "CON_table_4", CON_selectors[4])
    )
  )
  return(tab)
}

# Makes the tab for call source barplot
CON_Plot <- function(tabName, plotName, widgetName){
  tab <- tabPanel(tabName, # Tab title
    plotOutput(plotName),                 # plotOutput name
    # Graph controls
    selectInput(widgetName, "Selector", "Unselected", selected = 1))
  return(tab)
}

# Boolean to tell if the widgets have been loaded
widgetsLoaded <- FALSE

# Sets up the widgets based on whats in the data
## !!TODO: not working!! ##
CON_populate_Widgets <-function(session, input, data){
  if(widgetsLoaded){return()}
  # Selector widget for the source of call
  updateSelectInput(session, OFF_selectors[1], 
                    label = "Selector", 
                    choices = c("Unselected", unique(data$CallSource)), 
                    selected = "Unselected")
  
  # Selector widget for the police call status
  updateSelectInput(session, OFF_selectors[2], 
                    label = "Selector", 
                    choices = c("Unselected", unique(data$PoliceCallStatus)), 
                    selected = "Unselected")

  # Selector widget for the call priory
  updateSelectInput(session, OFF_selectors[3], 
                    label = "Selector", 
                    choices = c("Unselected", unique(data$PoliceCallPriority)), 
                    selected = "Unselected")

  # Selector widget for the city
  updateSelectInput(session, OFF_selectors[4], 
                    label = "Selector", 
                    choices = c("Unselected", unique(data$City)), 
                    selected = "Unselected")
  widgetsLoaded = TRUE
}