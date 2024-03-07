##################################################################
##################################################################
##              File for the call for Contacts                  ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
CON_topBar <- c("CON_dates", 
                "CON_Top_Selector"
)
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
CON_selectors <- c("CON_Selector_1", 
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

# Boolean to tell if the widgets have been loaded
CON_widgetsLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
CON_populate_Widgets <-function(session, Graph1_selector, Graph2_selector, Graph3_selector, Graph4_selector, Topbar_selector1){
  # Check in the widgets have already been loaded
  if(CON_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, CON_selectors[1], Graph1_selector, CON_selectors[1])
  Selector_Updater(session, CON_selectors[2], Graph2_selector, CON_selectors[2])
  Selector_Updater(session, CON_selectors[3], Graph3_selector, CON_selectors[3])
  Selector_Updater(session, CON_selectors[4], Graph4_selector, CON_selectors[4])
  Selector_Updater(session, CON_topBar[2], Topbar_selector1, CON_topBar[2])
  # Mark that the widgets have been loaded
  CON_widgetsLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
CON_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "CON",
  # Topbar area
    fluidRow(box(width = 12, 
      column(width = 3, dateRangeInput(CON_topBar[1], label = CON_topBar[1])),
      column(width = 2, selectInput(CON_topBar[2], CON_topBar[2], "Unselected", selected = 1)),
      )
    ),
    # Main graph area
    fluidRow(
      # Makes the first graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("TAB 1", "CON_table_1", CON_selectors[1]),
        Plot_Maker("TAB 2", "CON_table_2", CON_selectors[2])
      ),
      # Makes the second graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("TAB 3", "CON_table_3", CON_selectors[3]),
        Plot_Maker("TAB 4", "CON_table_4", CON_selectors[4])
      )
    )
  )
  return(tab)
}