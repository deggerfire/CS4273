##################################################################
##################################################################
##        File for the call for Offenses, Case Offenses         ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
OFF1_topBar <- c(
                "OFF1Year_Selector_By_CaseNumber"
)
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
OFF1_selectors <- c("OFF1_Selector_1", 
                   "OFF1_Selector_2" 
             )

# Render function for OFF1 (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
OFF1_render <- function(output, plot1, plot2){
  output$OFF1_table_1 <- plot1
  output$OFF1_table_2 <- plot2
}

# Boolean to tell if the widgets have been loaded
OFF1_widgetsLoaded <- FALSE
OFF1_topBarLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
OFF1_populate_Widgets <-function(session, Graph1_selector, Graph2_selector){
  # Check in the widgets have already been loaded
  if(OFF1_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, OFF1_selectors[1], Graph1_selector, "Counts")
  Selector_Updater(session, OFF1_selectors[2], Graph2_selector, "IBR Crime Code")
  # Mark that the widgets have been loaded
  OFF1_widgetsLoaded <<- TRUE
}
OFF1_populateTopBar <-function(session, numberOfYears)
{
  if(OFF1_topBarLoaded){return()}
  Selector_Updater(session, OFF1_topBar[1],numberOfYears, "Select Year")
  OFF1_topBarLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
OFF1_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "OFF1",
  # Topbar area
    fluidRow(box(width = 12, 
      column(width = 2, selectInput(OFF1_topBar[1], OFF1_topBar[1], "Unselected", selected = 1)),
      column(width = 2, selectInput(OFF1_selectors[1], OFF1_selectors[1], "Unselected", selected = 1)),
      column(width = 2, selectInput(OFF1_selectors[2], OFF1_selectors[2], "Unselected", selected = 1)),
      )
    ),
    # Main graph area
    fluidRow(
      # Makes the first graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("Counts", "OFF1_table_1"),
      ),
      # Makes the second graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("IBR Crime Code", "OFF1_table_2"),
      )
    )
  )
  return(tab)
}