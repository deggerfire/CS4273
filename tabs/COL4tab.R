##################################################################
##################################################################
##          File for the Collisions By injury                 ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
COL4_topBar <- c(
                 "COL4Select_Year"
)
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
COL4_selectors <- c("COL4_Selector_1", 
                    "COL4_Selector_2", 
                    "COL4_Selector_3", 
                    "COL4_Selector_4"
)

# Render function for COL4 (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
COL4_render <- function(output, plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, 
                        plot11, plot12, plot13){
  
  output$COL4_table_1 <- plot1
  output$COL4_table_2 <- plot2
  output$COL4_table_3 <- plot3
  output$COL4_table_4 <- plot4
  output$COL4_table_5 <- plot5
  output$COL4_table_6 <- plot6
  output$COL4_table_7 <- plot7
  output$COL4_table_8 <- plot8
  output$COL4_table_9 <- plot9
  output$COL4_table_10 <- plot10
  output$COL4_table_11 <- plot11
  output$COL4_table_12 <- plot12
  output$COL4_table_13 <- plot13
}

# Boolean to tell if the widgets have been loaded
COL4_widgetsLoaded <- FALSE
COL4_topBarLoaded <<- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
COL4_populate_Widgets <-function(session, Graph1_selector, Graph2_selector, Graph3_selector, Graph4_selector, Topbar_selector1){
  # Check in the widgets have already been loaded
  if(COL4_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, COL4_selectors[1], Graph1_selector, COL4_selectors[1])
  Selector_Updater(session, COL4_selectors[2], Graph2_selector, COL4_selectors[2])
  Selector_Updater(session, COL4_selectors[3], Graph3_selector, COL4_selectors[3])
  Selector_Updater(session, COL4_selectors[4], Graph4_selector, COL4_selectors[4])
  Selector_Updater(session, COL4_topBar[2], Topbar_selector1, COL4_topBar[2])
  # Mark that the widgets have been loaded
  COL4_widgetsLoaded <<- TRUE
}
COL4_poulateTopBar <-function(session, numberOfYears)
{
  if(COL4_topBarLoaded){return()}
  Selector_Updater(session, COL4_topBar[1],numberOfYears, COL4_topBar[1])
  COL4_topBarLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
COL4_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "COL4",
                 # Topbar area
                 fluidRow(box(width = 12, 
                              column(width = 2, selectInput(COL4_topBar[1], COL4_topBar[1], "Unselected", selected = 1)),
                 )
                 ),
                 # Main graph area
                 fluidRow(
                   # Makes the first graph area
                   tabBox(
                     height = "500px",
                     # Uses functions to make what is in each tab (string is the name of the plotOutput)
                     Plot_MakerWOSelect("Jan", "COL4_table_1"),
                     Plot_MakerWOSelect("Feb", "COL4_table_2"),
                     Plot_MakerWOSelect("Mar", "COL4_table_3"),
                     Plot_MakerWOSelect("Apr", "COL4_table_4"),
                     Plot_MakerWOSelect("May", "COL4_table_5"),
                     Plot_MakerWOSelect("Jun", "COL4_table_6"),
                     Plot_MakerWOSelect("Jul", "COL4_table_7"),
                     Plot_MakerWOSelect("Aug", "COL4_table_8"),
                     Plot_MakerWOSelect("Sep", "COL4_table_9"),
                     Plot_MakerWOSelect("Oct", "COL4_table_10"),
                     Plot_MakerWOSelect("Nov", "COL4_table_11"),
                     Plot_MakerWOSelect("Dec", "COL4_table_12")
                   ),
                   # Makes the second graph area
                   tabBox(
                     height = "500px",
                     # Uses functions to make what is in each tab (string is the name of the plotOutput)
                     Plot_MakerWOSelect("Throughout Year", "COL4_table_13")
                   )
                 )
  )
  return(tab)
}