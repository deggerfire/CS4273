##################################################################
##################################################################
##          File for the call for Offenses, arrests             ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
OFF4_topBar <- c( 
                "OFF4Year_Selector_By_CaseNumber"
)
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
OFF4_selectors <- c("OFF4_Selector_1", 
                   "OFF4_Selector_2", 
                   "OFF4_Selector_3", 
                   "OFF4_Selector_4"
             )

# Render function for OFF4 (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
OFF4_render <- function(output, plot1, plot2, plot3, plot4){
  output$OFF4_table_1 <- plot1
  output$OFF4_table_2 <- plot2
  output$OFF4_table_3 <- plot3
  output$OFF4_table_4 <- plot4
}

# Boolean to tell if the widgets have been loaded
OFF4_widgetsLoaded <- FALSE
OFF4_topBarLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
OFF4_populate_Widgets <-function(session, Graph1_selector, Graph2_selector, Graph3_selector, Graph4_selector){
  # Check in the widgets have already been loaded
  if(OFF4_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, OFF4_selectors[1], Graph1_selector, OFF4_selectors[1])
  Selector_Updater(session, OFF4_selectors[2], Graph2_selector, OFF4_selectors[2])
  Selector_Updater(session, OFF4_selectors[3], Graph3_selector, OFF4_selectors[3])
  Selector_Updater(session, OFF4_selectors[4], Graph4_selector, OFF4_selectors[4])
  # Mark that the widgets have been loaded
  OFF4_widgetsLoaded <<- TRUE
}
OFF4_poulateTopBar <-function(session, numberOfYears)
{
  if(OFF4_topBarLoaded){return()}
  Selector_Updater(session, OFF4_topBar[1],numberOfYears, OFF4_topBar[1])
  OFF4_topBarLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
OFF4_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "OFF4",
  # Topbar area
    fluidRow(box(width = 12, 
      column(width = 2, selectInput(OFF4_topBar[1], OFF4_topBar[1], "Unselected", selected = 1)),
      )
    ),
    # Main graph area
    fluidRow(
      # Makes the first graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("TAB 1", "OFF4_table_1", OFF4_selectors[1]),
        Plot_Maker("TAB 2", "OFF4_table_2", OFF4_selectors[2])
      ),
      # Makes the second graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("TAB 3", "OFF4_table_3", OFF4_selectors[3]),
        Plot_Maker("TAB 4", "OFF4_table_4", OFF4_selectors[4])
      )
    )
  )
  return(tab)
}