##################################################################
##################################################################
##             File for the call for service UI                 ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
CFS_topBar <- c( 
            "CFSSelect_Year"
)
# List of the widget id's on the screen under the graphs. This list does have functionally
# This should help with step 2
CFS_selectors <- c("CFS_Source_of_Call_Selector"    , 
                   "CFS_Police_Call_Status_Selector", 
                   "CFS_Police_Call_Priority_Selector", 
                   "CFS_City_Selector",
                   "CFS_Top_Selector"
             )

# Render function for call for service (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
CFS_render <- function(output, plot1, plot2, plot3, plot4){
  output$CFS_table_1 <- plot1
  output$CFS_table_2 <- plot2
  output$CFS_table_3 <- plot3
  output$CFS_table_4 <- plot4
}

# Boolean to tell if the widgets have been loaded
CFS_widgetsLoaded <- FALSE
CFS_topBarLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
CFS_populate_Widgets <-function(session, Graph1_selector, Graph2_selector, Graph3_selector, Graph4_selector){
  # Check in the widgets have already been loaded
  if(CFS_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, CFS_selectors[1], Graph1_selector, CFS_selectors[1])
  Selector_Updater(session, CFS_selectors[2], Graph2_selector, CFS_selectors[2])
  Selector_Updater(session, CFS_selectors[3], Graph3_selector, CFS_selectors[3])
  Selector_Updater(session, CFS_selectors[4], Graph4_selector, CFS_selectors[4])
  # Mark that the widgets have been loaded
  CFS_widgetsLoaded <<- TRUE
}
CFS_populateTopBar <-function(session, numberOfYears)
{
  if(CFS_topBarLoaded){return()}
  Selector_Updater(session, CFS_topBar[1],numberOfYears, CFS_topBar[1])
  CFS_topBarLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
CFS_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "CFS",
    # Topbar area
    fluidRow(box(width = 12, 
      column(width = 2, selectInput(CFS_topBar[1], CFS_topBar[1], "Unselected", selected = 1)),
      )
    ),
    # Main graph area
    fluidRow(
      # Makes the first graph area
      tabBox(
        width = 6,
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("Source of Call"    , "CFS_table_1", CFS_selectors[1]),
        Plot_Maker("Police Call Status", "CFS_table_2", CFS_selectors[2])
      ),
      # Makes the second graph area
      tabBox(
        width = 6,
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("Police Call Priority", "CFS_table_3", CFS_selectors[3]),
        Plot_Maker("City"              , "CFS_table_4", CFS_selectors[4])
      )
    )
  )
  return(tab)
}