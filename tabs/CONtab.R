##################################################################
##################################################################
##               File for the Contacts UI                       ##
##################################################################
##################################################################
# This file is for UI stuff related to Contacts Tab

## TODO: make this file do something

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
CON_tab <- function(){
  tab <- tabItem(tabName = "CON",
    tabBox(
      height = "500px",
      CON_Call_Source_BP("CON_table_1")
    )
  )
  return(tab)
}

# Makes the tab for call source barplot
CON_Call_Source_BP <- function(plotName){
  tab <- tabPanel('Call Source Bargraph', # Tab title
    plotOutput(plotName),                 # plotOutput name
    # Graph controls
    checkboxGroupInput("checkGroup", 
      label = h3("Checkbox group"), 
      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}