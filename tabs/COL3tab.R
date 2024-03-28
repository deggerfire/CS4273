##################################################################
##################################################################
##          File for the Collisions By Location                 ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
COL3_topBar <- c("COL3_dates", 
                 "COL3_Top_Selector"
)
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
COL3_selectors <- c("COL3_Selector_1", 
                    "COL3_Selector_2", 
                    "COL3_Selector_3", 
                    "COL3_Selector_4"
)

# Render function for COL3 (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
COL3_render <- function(output, plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10){
  output$COL3_table_1 <- plot1 #Bar Graph of Streets A-B
  output$COL3_table_2 <- plot2 #Bar Graph of Streets C-G
  output$COL3_table_3 <- plot3 #Bar Graph of Streets H-L
  output$COL3_table_4 <- plot4 #Bar Graph of Streets M-Z
  output$COL3_table_5 <- plot5 #Pie Chart of Streets A-B
  output$COL3_table_6 <- plot6 #Pie Chart of Streets C-G
  output$COL3_table_7 <- plot7 #Pie Chart of Streets H-L
  output$COL3_table_8 <- plot8 #Pie Chart of Streets M-Z
  output$COL3_table_9 <- plot9 #Bar Graph showing main streets
  output$COL3_table_10 <- plot10 #Pie Chart showing main streets

}

# Boolean to tell if the widgets have been loaded
COL3_widgetsLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
COL3_populate_Widgets <-function(session, Graph1_selector, Graph2_selector, Graph3_selector, Graph4_selector, Topbar_selector1){
  # Check in the widgets have already been loaded
  if(COL3_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, COL3_selectors[1], Graph1_selector, COL3_selectors[1])
  Selector_Updater(session, COL3_selectors[2], Graph2_selector, COL3_selectors[2])
  Selector_Updater(session, COL3_selectors[3], Graph3_selector, COL3_selectors[3])
  Selector_Updater(session, COL3_selectors[4], Graph4_selector, COL3_selectors[4])
  Selector_Updater(session, COL3_topBar[2], Topbar_selector1, COL3_topBar[2])
  # Mark that the widgets have been loaded
  COL3_widgetsLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
COL3_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "COL3",
                 # Topbar area
                 fluidRow(box(width = 12, 
                              column(width = 3, dateRangeInput(COL3_topBar[1], label = COL3_topBar[1])),
                              column(width = 2, selectInput(COL3_topBar[2], COL3_topBar[2], "Unselected", selected = 1)),
                 )
                 ),
                 # Main graph area
                 fluidRow(
                   # Makes the first graph area
                   tabBox(
                     height = "500px",
                     # Uses functions to make what is in each tab (string is the name of the plotOutput)
                     Plot_MakerWOSelect("Streets A-B", "COL3_table_1"), #Outputs bargraph of Streets A-B
                     Plot_MakerWOSelect("Streets C-G", "COL3_table_2"), #Outputs bargraph of Streets C-G
                     Plot_MakerWOSelect("Streets H-L", "COL3_table_3"), #Outputs bargraph of Streets H-L
                     Plot_MakerWOSelect("Streets M-Z", "COL3_table_4"), #Outputs bargraph of Streets M-Z
                     Plot_MakerWOSelect("Common Streets", "COL3_table_9") #Outputs bargraph of Common streets
                   ),
                   # Makes the second graph area
                   tabBox(
                     height = "500px",
                     # Uses functions to make what is in each tab (string is the name of the plotOutput)
                     Plot_MakerWOSelect("Streets A-B", "COL3_table_5"), #Outputs Pie Chart of Streets A-B
                     Plot_MakerWOSelect("Streets C-G", "COL3_table_6"), #Outputs Pie Chart of Streets C-G
                     Plot_MakerWOSelect("Streets H-L", "COL3_table_7"), #Outputs Pie Chart of Streets H-L
                     Plot_MakerWOSelect("Streets M-Z", "COL3_table_8"), #Outputs Pie Chart of Streets M-Z
                     Plot_MakerWOSelect("Common Streets", "COL3_table_10") #Outputs Pie Chart of Common streets
                   )
                 )
  )
  return(tab)
}