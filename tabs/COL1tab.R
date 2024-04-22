##################################################################
##################################################################
##          File for the Collisions By Severity                 ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
COL1_topBar <- c( 
                "COL1Select_Year"
)
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
COL1_selectors <- c("COL1_Selector_1", 
                   "COL1_Selector_2", 
                   "COL1_Selector_3", 
                   "COL1_Selector_4"
             )

# Render function for Col1 (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
COL1_render <- function(output, plot1, plot2, plot3, plot4){
  output$COL1_table_1 <- plot1
  output$COL1_table_2 <- plot2
  output$COL1_table_3 <- plot3
  output$COL1_table_4 <- plot4
}

# Boolean to tell if the widgets have been loaded
COL1_widgetsLoaded <- FALSE
COL1_topBarLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
COL1_populate_Widgets <-function(session, Graph1_selector, Graph2_selector, Graph3_selector, Graph4_selector){
  # Check in the widgets have already been loaded
  if(COL1_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, COL1_selectors[1], Graph1_selector, "Unit Type")
  Selector_Updater(session, COL1_selectors[2], Graph2_selector, "Driver Condition")
  Selector_Updater(session, COL1_selectors[3], Graph3_selector, "Chemical Test")
  Selector_Updater(session, COL1_selectors[4], Graph4_selector, "Contributing Factors")
  # Mark that the widgets have been loaded
  COL1_widgetsLoaded <<- TRUE
}
COL1_populateTopBar <-function(session, numberOfYears)
{
  if(COL1_topBarLoaded){return()}
  Selector_Updater(session, COL1_topBar[1],numberOfYears, COL1_topBar[1])
  COL1_topBarLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
COL1_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "COL1",
  # Topbar area
    fluidRow(box(width = 12, 
      column(width = 2, selectInput(COL1_topBar[1],"Select Year", "Unselected", selected = 1)),
      )
    ),
    # Main graph area
    fluidRow(
      # Makes the first graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("Unit Type", "COL1_table_1", COL1_selectors[1]),
        Plot_Maker("Driver Ped Condition", "COL1_table_2", COL1_selectors[2])
      ),
      # Makes the second graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("Chem Test", "COL1_table_3", COL1_selectors[3]),
        Plot_Maker("Contr. Factors", "COL1_table_4", COL1_selectors[4])
      )
    )
  )
  return(tab)
}