##################################################################
##################################################################
##             File for the Collisions UI                       ##
##################################################################
##################################################################
# This file is for UI stuff related to Collisions Tab

## TODO: make this file do something

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top

COL_render <- function(output, plot1, plot2, plot3, plot4){
  output$COL_table_1 <- plot1
  output$COL_table_2 <- plot2
  output$COL_table_3 <- plot3
  output$COL_table_4 <- plot4
}

COL_tab <- function(){
  tab <- tabItem(tabName = "COL",
    tabBox(
      height = "500px",
      COL_Call_Source_BP("COL_table_1")
    )
  )
  return(tab)
}

# Makes the tab for call source barplot
COL_Call_Source_BP <- function(plotName){
  tab <- tabPanel('Call Source Bargraph', # Tab title
    plotOutput(plotName),                 # plotOutput name
    # Graph controls
    checkboxGroupInput("checkGroup", 
      label = h3("Checkbox group"), 
      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}