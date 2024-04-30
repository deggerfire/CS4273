##################################################################
##################################################################
##             File for the call for service UI                 ##
##################################################################
##################################################################
source("tabs/UIHelperFunctions.R")
# List of the widget id's on the screen in the top bar. This list does have functionally
# This should help with step 2
UOF_topBar <- c( 
  "UOFSelect_Year"
)
# List of the widget id's on the screen under the graphs. This list does have functionally
# This should help with step 2
UOF_selectors <- c("UOF_Race_Selector"    , 
                   "UOF_Sex_Selector", 
                   "UOF_Years_Employed_Selector", 
                   "UOF_Involvement_Selector",
                   "UOF_Age_Selector",
                   "UOF_Subject_Type_Selector",
                   "UOF_Top_Selector"
)

# Render function for call for service (puts graphs on screen)
# This function is for step 4
#   output: this is the output variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   plot1, ..., plotx: a graph to be put in that spot on screen
#     goes from upper left to lower right order
#     if you need the format of the graphs change ask UI person
UOF_render <- function(output, plot1, plot2, plot3, plot4, plot5, plot6){
  output$UOF_table_1 <- plot1
  output$UOF_table_2 <- plot2
  output$UOF_table_3 <- plot3
  output$UOF_table_4 <- plot4
  output$UOF_table_5 <- plot5
  output$UOF_table_6 <- plot6
}

# Boolean to tell if the widgets have been loaded
UOF_widgetsLoaded <- FALSE
UOF_topBarLoaded <- FALSE

# Sets of the selectors based on the inputted data
# This function is the setup for the conditions in step 2
#   session:  this is the session variable passes into the server function
#     see this line in app.R (use ctrl+f) "server <- function(input, output, session) {"
#   selector1Data, ..., selectorxData: The data that will be put in the selectors
#     goes from upper left to lower right order
UOF_populate_Widgets <-function(session, Graph1_selector, Graph2_selector, Graph3_selector, Graph4_selector, Graph5_selector, Graph6_selector){
  # Check in the widgets have already been loaded
  if(UOF_widgetsLoaded){return()}
  # Populate the widgets with each of the unique values in the given data
  Selector_Updater(session, UOF_selectors[1], Graph1_selector, "Race")
  Selector_Updater(session, UOF_selectors[2], Graph2_selector, "Sex")
  Selector_Updater(session, UOF_selectors[3], Graph3_selector, "Years Employed")
  Selector_Updater(session, UOF_selectors[4], Graph4_selector, "Involvement")
  Selector_Updater(session, UOF_selectors[5], Graph5_selector, "Age")
  Selector_Updater(session, UOF_selectors[6], Graph6_selector, "Subject Type")
  # Mark that the widgets have been loaded
  UOF_widgetsLoaded <<- TRUE
}
UOF_populateTopBar <-function(session, numberOfYears)
{
  if(UOF_topBarLoaded){return()}
  Selector_Updater(session, UOF_topBar[1],numberOfYears, "Select Year")
  UOF_topBarLoaded <<- TRUE
}

##################################################################
##          Everything below this point is UI stuff             ##
##    odds are what you are looking for is not down here        ##
##################################################################

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
UOF_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "UOF",
                 # Topbar area
                 fluidRow(box(width = 12, 
                              column(width = 2, selectInput(UOF_topBar[1], UOF_topBar[1], "Unselected", selected = 1)),
                              column(width = 2, selectInput(UOF_selectors[1], UOF_selectors[1], "Unselected", selected = 1)),
                              column(width = 2, selectInput(UOF_selectors[2], UOF_selectors[2], "Unselected", selected = 1)),
                              column(width = 2, selectInput(UOF_selectors[3], UOF_selectors[3], "Unselected", selected = 1)),
                              column(width = 2, selectInput(UOF_selectors[4], UOF_selectors[4], "Unselected", selected = 1)),
                              column(width = 2, selectInput(UOF_selectors[5], UOF_selectors[5], "Unselected", selected = 1)),
                              column(width = 2, selectInput(UOF_selectors[6], UOF_selectors[6], "Unselected", selected = 1)),
                 )
                 ),
                 # Main graph area
                 fluidRow(
                   # Makes the first graph area
                   tabBox(
                     width = 6,
                     # Uses functions to make what is in each tab (string is the name of the plotOutput)
                     Plot_Maker("Race"    , "UOF_table_1"),
                     Plot_Maker("Sex", "UOF_table_2")
                   ),
                   # Makes the second graph area
                   tabBox(
                     width = 6,
                     # Uses functions to make what is in each tab (string is the name of the plotOutput)
                     Plot_Maker("Years Employed", "UOF_table_3"),
                     Plot_Maker("Involvement", "UOF_table_4"),
                     Plot_Maker("Age", "UOF_table_5"),
                     Plot_Maker("Subject Type"              , "UOF_table_6")
                   )
                 )
  )
  return(tab)
}