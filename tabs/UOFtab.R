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
        UOF_Race_PC("UOF_table_1"),
        UOF_Sex_PC("UOF_table_2")
      ),
     tabBox(
       height = "500px",
       UOF_Involvement_BP("UOF_table_3"),
       UOF_Subject_Type_BP("UOF_table_4")
     )
    )
  return(tab)
}

# Makes the tab for race piechart
UOF_Race_PC <- function(plotName){
  tab <- tabPanel('Race Piechart', # Tab title
    plotOutput(plotName),                 # plotOutput name
    # Graph controls
    checkboxGroupInput("checkGroup", 
      label = h3("Checkbox group"), 
      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}

# Makes the tab for sex piechart
UOF_Sex_PC <- function(plotName){
  tab <- tabPanel('Sex Piechart', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Checkbox group"), 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}

# Makes the tab for call source barplot
UOF_Involvement_BP <- function(plotName){
  tab <- tabPanel('Involvement Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Checkbox group"), 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}


# Makes the tab for call source barplot
UOF_Subject_Type_BP <- function(plotName){
  tab <- tabPanel('Subject Type Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Checkbox group"), 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}


# Temporary render function TODO: Work out how data team wants to pass in
UOF_render <- function(output, plot1, plot2, plot3, plot4){
  output$UOF_table_1 <- plot1
  output$UOF_table_2 <- plot2
  output$UOF_table_3 <- plot3
  output$UOF_table_4 <- plot4
}