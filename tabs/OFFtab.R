##################################################################
##################################################################
##               File for the Offenses UI                       ##
##################################################################
##################################################################
# This file is for UI stuff related to Offenses Tab

## TODO: make this file do something

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
OFF_tab <- function(){
  tab <- tabItem(tabName = "OFF",
    tabBox(
      height = "500px",
      OFF_Call_Source_BP("OFF_table_1")
    )
  )
  return(tab)
}

# Makes the tab for call source barplot
OFF_Call_Source_BP <- function(plotName){
  tab <- tabPanel('Call Source Bargraph', # Tab title
    plotOutput(plotName),                 # plotOutput name
    # Graph controls
    checkboxGroupInput("checkGroup", 
      label = h3("Checkbox group"), 
      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1),
      "The Offenses data set includes information from our records system regarding the offense description and the demographic breakdown of both the community members that interact with the police and the police themselves. This data set includes case details, subject breakdown, and arrest details.")
  return(tab)
}