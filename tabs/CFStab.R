##################################################################
##################################################################
##             File for the call for service UI                 ##
##################################################################
##################################################################
# This file is for UI stuff related to Call for Service Tab
CFS_selectors <- c("CFS_Source_of_Call_Selector"    , 
                   "CFS_Police_Call_Status_Selector", 
                   "CFS_Police_Call_Prioty_Selector", 
                   "CFS_City_Selector"
             )

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
CFS_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "CFS",
    # Makes the first graph area
    tabBox(
      height = "500px",
      # Uses functions to make what is in each tab (string is the name of the plotOutput)
      CFS_Plot("Source of Call"    , "CFS_table_1", "CFS_Source_of_Call_Selector"),
      CFS_Plot("Police Call Status", "CFS_table_2", "CFS_Police_Call_Status_Selector")
    ),
    # Makes the second graph area
    tabBox(
      height = "500px",
      # Uses functions to make what is in each tab (string is the name of the plotOutput)
      CFS_Plot("Police Call Prioty", "CFS_table_3", "CFS_Police_Call_Prioty_Selector"),
      CFS_Plot("City"              , "CFS_table_4", "CFS_City_Selector")
    )
  )
  return(tab)
}

# Makes the tab for call source barplot
CFS_Plot <- function(tabName, plotName, widgetName){
  tab <- tabPanel(tabName, # Tab title
    plotOutput(plotName),                 # plotOutput name
    # Graph controls
    selectInput(widgetName, "Selector", "Unselected", selected = 1))
  return(tab)
}

# Sets up the widgets based on whats in the data
CFS_populate_Widgets <-function(session, input, data){
  if(input$CFS_Source_of_Call_Selector != "Unselected"){return()}
  # Selector widget for the source of call
  updateSelectInput(session, "CFS_Source_of_Call_Selector", 
                    label = "Selector", 
                    choices = c("Unselected", unique(data$CallSource)), 
                    selected = "Unselected")
  
  # Selector widget for the police call status
  updateSelectInput(session, "CFS_Police_Call_Status_Selector", 
                    label = "Selector", 
                    choices = c("Unselected", unique(data$PoliceCallStatus)), 
                    selected = "Unselected")

  # Selector widget for the call priory
  updateSelectInput(session, "CFS_Police_Call_Prioty_Selector", 
                    label = "Selector", 
                    choices = c("Unselected", unique(data$PoliceCallPriority)), 
                    selected = "Unselected")

  # Selector widget for the city
  updateSelectInput(session, "CFS_City_Selector", 
                    label = "Selector", 
                    choices = c("Unselected", unique(data$City)), 
                    selected = "Unselected")
}

# Temporary render function TODO: Work out how data team wants to pass in
CFS_render <- function(output, plot1, plot2, plot3, plot4){
  output$CFS_table_1 <- plot1
  output$CFS_table_2 <- plot2
  output$CFS_table_3 <- plot3
  output$CFS_table_4 <- plot4
}