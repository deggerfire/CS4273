##################################################################
##################################################################
##              File for the call for Offenses                  ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
OFF_selectors <- c("OFF_Selector_1"    , 
                   "OFF_Selector_2", 
                   "OFF_Selector_3", 
                   "OFF_Selector_4"
             )

# Render function for call for service (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
OFF_render <- function(output, plot1, plot2, plot3, plot4){
  output$OFF_table_1 <- plot1
  output$OFF_table_2 <- plot2
  output$OFF_table_3 <- plot3
  output$OFF_table_4 <- plot4
}

# Boolean to tell if the widgets have been loaded
OFF_widgetsLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
OFF_populate_Widgets <-function(session, selector1Data, selector2Data, selector3Data, selector4Data){
  # Check in the widgets have already been loaded
  if(OFF_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, OFF_selectors[1], selector1Data)
  Selector_Updater(session, OFF_selectors[2], selector2Data)
  Selector_Updater(session, OFF_selectors[3], selector3Data)
  Selector_Updater(session, OFF_selectors[4], selector4Data)
  # Mark that the widgets have been loaded
  OFF_widgetsLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
OFF_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "OFF",
    # Makes the first graph area
    tabBox(
      height = "500px",
      # Uses functions to make what is in each tab (string is the name of the plotOutput)
      Plot_Maker("TAB 1", "OFF_table_1", OFF_selectors[1]),
      Plot_Maker("TAB 2", "OFF_table_2", OFF_selectors[2])
    ),
    # Makes the second graph area
    tabBox(
      height = "500px",
      # Uses functions to make what is in each tab (string is the name of the plotOutput)
      Plot_Maker("TAB 3", "OFF_table_3", OFF_selectors[3]),
      Plot_Maker("TAB 4", "OFF_table_4", OFF_selectors[4])
    )
  )
  return(tab)
}