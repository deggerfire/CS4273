##################################################################
##################################################################
##             File for the call for service UI                 ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
CFS_selectors <- c("CFS_Source_of_Call_Selector"    , 
                   "CFS_Police_Call_Status_Selector", 
                   "CFS_Police_Call_Prioty_Selector", 
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

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
CFS_populate_Widgets <-function(session, selector1Data, selector2Data, selector3Data, selector4Data, selector5Data){
  # Check in the widgets have already been loaded
  if(CFS_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, CFS_selectors[1], selector1Data)
  Selector_Updater(session, CFS_selectors[2], selector2Data)
  Selector_Updater(session, CFS_selectors[3], selector3Data)
  Selector_Updater(session, CFS_selectors[4], selector4Data)
  Selector_Updater(session, CFS_selectors[5], selector5Data)
  # Mark that the widgets have been loaded
  CFS_widgetsLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
CFS_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "CFS",
    fluidRow(box(width = 12, 
      column(width = 3, dateRangeInput("CFS_date", label = "Date range")),
      column(width = 2, selectInput(CFS_selectors[5], "Selector", "Unselected", selected = 1)),
      )
    ),
    # Makes the first graph area
    fluidRow(
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
        Plot_Maker("Police Call Prioty", "CFS_table_3", CFS_selectors[3]),
        Plot_Maker("City"              , "CFS_table_4", CFS_selectors[4])
      )
    )
  )
  return(tab)
}