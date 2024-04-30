##################################################################
#################################################################
##  File for the call for Complaints and Inquiries              ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
CI_topBar <- c(
  "CISelect_Year"
)
# List of the widget id's on the screen. This list does have functionally
# This should help with step 2
CI_selectors <- c("CI_Race_Selector", 
                  "CI_Sex_Selector", 
                  "CI_Years_Employed_Selector", 
                  "CI_Allegations_Selector",
                  "CI_Involvement_Selector",
                  "CI_Age_Selector",
                  "CI_Subject_Type_Selector"
)

# Render function for call for service (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
CI_render <- function(output, plot1, plot2, plot3, plot4, plot5, plot6, plot7){
  output$CI_table_1 <- plot1
  output$CI_table_2 <- plot2
  output$CI_table_3 <- plot3
  output$CI_table_4 <- plot4
  output$CI_table_5 <- plot5
  output$CI_table_6 <- plot6
  output$CI_table_7 <- plot7
  
}

# Boolean to tell if the widgets have been loaded
CI_widgetsLoaded <- FALSE
CI_topBarLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
CI_populate_Widgets <-function(session, Graph1_selector, Graph2_selector, Graph3_selector, Graph4_selector, Graph5_selector, Graph6_selector, Graph7_selector){
  # Check in the widgets have already been loaded
  if(CI_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, CI_selectors[1], Graph1_selector, "Race")
  Selector_Updater(session, CI_selectors[2], Graph2_selector, "Sex")
  Selector_Updater(session, CI_selectors[3], Graph3_selector, "Years Employed")
  Selector_Updater(session, CI_selectors[4], Graph4_selector, "Allegations")
  Selector_Updater(session, CI_selectors[5], Graph5_selector, "Involvement")
  Selector_Updater(session, CI_selectors[6], Graph6_selector, "Age")
  Selector_Updater(session, CI_selectors[7], Graph7_selector, "Subject Type")
  
  
  # Mark that the widgets have been loaded
  CI_widgetsLoaded <<- TRUE
}
CI_populateTopBar <-function(session, numberOfYears)
{
  if(CI_topBarLoaded){return()}
  Selector_Updater(session, CI_topBar[1],numberOfYears, "Select Year")
  CI_topBarLoaded <<- TRUE
}


##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
CI_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "CI",
                 # Topbar area
                 fluidRow(box(width = 12, 
                              column(width = 2, selectInput(CI_topBar[1], CI_topBar[1], "Unselected", selected = 1)),
                              column(width = 2, selectInput(CI_selectors[1], CI_selectors[1], "Unselected", selected = 1)),
                              column(width = 2, selectInput(CI_selectors[2], CI_selectors[2], "Unselected", selected = 1)),
                              column(width = 2, selectInput(CI_selectors[3], CI_selectors[3], "Unselected", selected = 1)),
                              column(width = 2, selectInput(CI_selectors[4], CI_selectors[4], "Unselected", selected = 1)),
                              column(width = 2, selectInput(CI_selectors[5], CI_selectors[5], "Unselected", selected = 1)),
                              column(width = 2, selectInput(CI_selectors[6], CI_selectors[6], "Unselected", selected = 1)),
                              column(width = 2, selectInput(CI_selectors[7], CI_selectors[7], "Unselected", selected = 1)),
                 )
                 ),
                 # Main graph area
                 fluidRow(
                   # Makes the first graph area
                   tabBox(
                     height = "500px",
                     # Uses functions to make what is in each tab (string is the name of the plotOutput)
                     Plot_Maker("Race", "CI_table_1"),
                     Plot_Maker("Sex", "CI_table_2"),
                     
                   ),
                   # Makes the second graph area
                   tabBox(
                     height = "400px",
                     # Uses functions to make what is in each tab (string is the name of the plotOutput)
                     Plot_Maker("Years Employed", "CI_table_3"),
                     Allegations_Plot_Maker("Allegations", "CI_table_4"),
                     Plot_Maker("Involvement", "CI_table_5"),
                     Plot_Maker("Age", "CI_table_6"),
                     Plot_Maker("Subject_Type", "CI_table_7")
                   )
                 )
  )
  return(tab)
}