##################################################################
##################################################################
##          File for the call for Offenses, subjects            ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
OFF3_topBar <- c(
                "OFF3Year_Selector_By_CaseNumber"
)
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
OFF3_selectors <- c("OFF3_Selector_1", 
                   "OFF3_Selector_2", 
                   "OFF3_Selector_3", 
                   "OFF3_Selector_4"
             )

# Render function for OFF3 (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
OFF3_render <- function(output, plot1, plot2, plot3, plot4){
  output$OFF3_table_1 <- plot1
  output$OFF3_table_2 <- plot2
  output$OFF3_table_3 <- plot3
  output$OFF3_table_4 <- plot4
}

# Boolean to tell if the widgets have been loaded
OFF3_widgetsLoaded <- FALSE
OFF3_topBarLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
OFF3_populate_Widgets <-function(session, Graph1_selector, Graph2_selector, Graph3_selector, Graph4_selector){
  # Check in the widgets have already been loaded
  if(OFF3_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, OFF3_selectors[1], Graph1_selector, "Sex")
  Selector_Updater(session, OFF3_selectors[2], Graph2_selector, "Race")
  Selector_Updater(session, OFF3_selectors[3], Graph3_selector, "Type")
  Selector_Updater(session, OFF3_selectors[4], Graph4_selector, "Sub-Type")
  # Mark that the widgets have been loaded
  OFF3_widgetsLoaded <<- TRUE
}
OFF3_populateTopBar <-function(session, numberOfYears)
{
  if(OFF3_topBarLoaded){return()}
  Selector_Updater(session, OFF3_topBar[1],numberOfYears, "Select Year")
  OFF3_topBarLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
OFF3_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "OFF3",
  # Topbar area
    fluidRow(box(width = 12, 
      column(width = 2, selectInput(OFF3_topBar[1], OFF3_topBar[1], "Unselected", selected = 1)),
      )
    ),
    # Main graph area
    fluidRow(
      # Makes the first graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("TAB 1", "OFF3_table_1", OFF3_selectors[1]),
        Plot_Maker("TAB 2", "OFF3_table_2", OFF3_selectors[2])
      ),
      # Makes the second graph area
      tabBox(
        height = "500px",
        # Uses functions to make what is in each tab (string is the name of the plotOutput)
        Plot_Maker("TAB 3", "OFF3_table_3", OFF3_selectors[3]),
        Plot_Maker("TAB 4", "OFF3_table_4", OFF3_selectors[4])
      )
    )
  )
  return(tab)
}