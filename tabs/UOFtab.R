##################################################################
##################################################################
##             File for the Use of force UI                     ##
##################################################################
##################################################################
# This file is for UI stuff related to Use of force Tab

## TODO: make this file do something

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
UOF_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "UOF",
     tabBox(
      height = "500px",
        UOF_Call_Source_BP("UOF_table_1"),
        UOF_Call_Source_BP("UOF_table_2")
      )
    )
  return(tab)
}

# Makes the tab for call source barplot
UOF_Call_Source_BP <- function(plotName){
  tab <- tabPanel('Call Source Bargraph', # Tab title
    plotOutput(plotName),                 # plotOutput name
    # Graph controls
    checkboxGroupInput("checkGroup", 
      label = h3("Checkbox group"), 
      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}