##################################################################
##################################################################
##          File for the Collisions By injury                 ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
COL2_topBar <- c(
                "COL2Select_Year"
)
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
COL2_selectors <- c("COL2_Selector_1", 
                   "COL2_Selector_2"
             )

# Render function for Col2 (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
COL2_render <- function(output, plot1, plot2, plot3, plot4){
  output$COL2_table_1 <- plot1
  output$COL2_table_2 <- plot2
}

# Boolean to tell if the widgets have been loaded
COL2_widgetsLoaded <- FALSE
COL2_topBarLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
COL2_populate_Widgets <-function(session, Graph1_selector, Graph3_selector){
  # Check in the widgets have already been loaded
  if(COL2_widgetsLoaded){return()}
  ## Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, COL2_selectors[1], Graph1_selector, "Outcome")
  Selector_Updater(session, COL2_selectors[2], Graph3_selector, "Severity Number")
  # Mark that the widgets have been loaded
  COL2_widgetsLoaded <<- TRUE
}
COL2_populateTopBar <-function(session, numberOfYears)
{
  if(COL2_topBarLoaded){return()}
  Selector_Updater(session, COL2_topBar[1],numberOfYears, "Select Year")
  COL2_topBarLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
COL2_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "COL2",
  # Topbar area
    fluidRow(box(width = 12, 
      column(width = 2, selectInput(COL2_topBar[1], COL2_topBar[1], "Unselected", selected = 1)),
      column(width = 2, selectInput(COL2_selectors[1], COL2_selectors[1], "Unselected", selected = 1)),
      column(width = 2, selectInput(COL2_selectors[2], COL2_selectors[2], "Unselected", selected = 1)),
      )
    ),
    # Main graph area
    fluidRow(
      # Makes the first graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("Outcome", "COL2_table_1")
      ),
      # Makes the second graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("Severity Number", "COL2_table_2")
      )
    )
  )
  return(tab)
}