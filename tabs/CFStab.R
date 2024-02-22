##################################################################
##################################################################
##             File for the call for service UI                 ##
##################################################################
##################################################################
# This file is for UI stuff related to Call for Service Tab

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
CFS_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "CFS",
    # Makes the first graph area
    tabBox(
      height = "500px",
      # Uses functions to make what is in each tab (string is the name of the plotOutput)
      CFS_Call_Source_BP("CFS_table_1"),
      CFS_Call_Source_PC("CFS_table_2"),
      CFS_Call_Priority_BP("CFS_table_3"),
      CFS_Call_Priority_PC("CFS_table_4")
    ),
    # Makes the second graph area
    tabBox(
      height = "500px",
      # Uses functions to make what is in each tab (string is the name of the plotOutput)
      CFS_Call_Source_BP("CFS_table_5"),
      CFS_Call_Source_PC("CFS_table_6"),
      CFS_Call_Priority_BP("CFS_table_7"),
      CFS_Call_Priority_PC("CFS_table_8")
    )
  )
  return(tab)
}

# Makes the tab for call source barplot
CFS_Call_Source_BP <- function(plotName){
  tab <- tabPanel('Call Source Bargraph', # Tab title
    plotOutput(plotName),                 # plotOutput name
    # Graph controls
    checkboxGroupInput("checkGroup", 
      label = h3("Checkbox group"), 
      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}

# Makes the tab for call Status piechart
CFS_Call_Source_PC <- function(plotName){
  tab <- tabPanel('Call Status piechart', # Tab title
    plotOutput(plotName),                 # plotOutput name
    # Graph controls
    dateRangeInput("dates", label = "Date range"),
     sliderInput("slider2", label = h3("Slider Range"), min = 2014, 
        max = 2025, value = c(2022, 2023))
    )
  return(tab)
}

# Makes the tab for Call Priority barplot
CFS_Call_Priority_BP <- function(plotName){
  tab <- tabPanel('Call Priority Bargraph', # Tab title
    plotOutput(plotName),                   # plotOutput name
    # Graph controls
    radioButtons("checkGroup", 
      label = h3("Checkbox group"), 
      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}

# Makes the tab for Call Priority piechart
CFS_Call_Priority_PC <- function(plotName){
  tab <- tabPanel('City piechart', # Tab title
    plotOutput(plotName),          # plotOutput name
    # Graph controls
    selectInput("select", label = h3("Select box"), 
    choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
    selected = 1)
    )
  return(tab)
}

# Makes the tab for Call Priority piechart
CFS_Zip_BP <- function(plotName){
  tab <- tabPanel('Zip Piechart', # Tab title
                  plotOutput(plotName),          # plotOutput name
                  # Graph controls
                  selectInput("select", label = h3("Select box"), 
                              choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                              selected = 1)
  )
  return(tab)
}

# Temporary render function TODO: Work out how data team wants to pass in
CFS_render <- function(output, plot1, plot2, plot3, plot4, plot5){
  output$CFS_table_1 <- plot1
  output$CFS_table_2 <- plot2
  output$CFS_table_3 <- plot3
  output$CFS_table_4 <- plot4
  
  output$CFS_table_5 <- plot1
  output$CFS_table_6 <- plot2
  output$CFS_table_7 <- plot3
  output$CFS_table_8 <- plot4
}