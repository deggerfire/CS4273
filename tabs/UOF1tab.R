##################################################################
##################################################################
##              File for the call for UOF1                      ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
UOF1_topBar <- c("UOF1_dates", 
                "UOF1_Top_Selector"
)
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
UOF1_selectors <- c("UOF1_Selector_1", 
                   "UOF1_Selector_2", 
                   "UOF1_Selector_3", 
                   "UOF1_Selector_4"
             )

# Render function for call for service (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
UOF1_render <- function(output, plot1, plot2, plot3, plot4){
  output$UOF1_table_1 <- plot1
  output$UOF1_table_2 <- plot2
  output$UOF1_table_3 <- plot3
  output$UOF1_table_4 <- plot4
}

# Boolean to tell if the widgets have been loaded
UOF1_widgetsLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
UOF1_populate_Widgets <-function(session, Graph1_selector, Graph2_selector, Graph3_selector, Graph4_selector, Topbar_selector1){
  # Check in the widgets have already been loaded
  if(UOF1_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, UOF1_selectors[1], Graph1_selector, UOF1_selectors[1])
  Selector_Updater(session, UOF1_selectors[2], Graph2_selector, UOF1_selectors[2])
  Selector_Updater(session, UOF1_selectors[3], Graph3_selector, UOF1_selectors[3])
  Selector_Updater(session, UOF1_selectors[4], Graph4_selector, UOF1_selectors[4])
  Selector_Updater(session, UOF1_topBar[2], Topbar_selector1, UOF1_topBar[2])
  # Mark that the widgets have been loaded
  UOF1_widgetsLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
UOF1_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "UOF1",
  # Topbar area
    fluidRow(box(width = 12, 
      column(width = 3, dateRangeInput(UOF1_topBar[1], label = UOF1_topBar[1])),
      column(width = 2, selectInput(UOF1_topBar[2], UOF1_topBar[2], "Unselected", selected = 1)),
      )
    ),
    # Main graph area
    fluidRow(
      # Makes the first graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("TAB 1", "UOF1_table_1", UOF1_selectors[1]),
        Plot_Maker("TAB 2", "UOF1_table_2", UOF1_selectors[2])
      ),
      # Makes the second graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("TAB 3", "UOF1_table_3", UOF1_selectors[3]),
        Plot_Maker("TAB 4", "UOF1_table_4", UOF1_selectors[4])
      )
    )
  )
  return(tab)
}