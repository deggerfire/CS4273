##################################################################
##################################################################
##             File for the Use of force UI                     ##
##################################################################
##################################################################
# This file is for UI stuff related to Use of force Tab

## TODO: make this file do something

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
CI_tab <- function(){
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "CI",
                 tabBox(
                   height = "500px",
                   CI_Race_PC("CI_table_1"),
                   CI_Sex_PC("CI_table_2")
                 ),
                 tabBox(
                   height = "500px",
                   CI_Years_Employed_BP("CI_table_3"),
                   CI_Allegations_BP("CI_table_4"),
                   CI_Involvement_BP("CI_table_5"),
                   CI_Age_BP("CI_table_6"),
                   CI_Subject_Type_BP("CI_table_7")
                 )
  )
  return(tab)
}

# --- Pie Charts ---

# Makes the tab for race piechart
CI_Race_PC <- function(plotName){
  tab <- tabPanel('Race Piechart', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Checkbox group"), 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}

# Makes the tab for sex piechart
CI_Sex_PC <- function(plotName){
  tab <- tabPanel('Sex Piechart', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Checkbox group"), 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}


# --- Bar Charts ---

# Makes the tab for subject's years employed barplot

CI_Years_Employed_BP <- function(plotName){
  tab <- tabPanel('Years Employed Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Checkbox group"), 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}


# Makes the tab for allegations barplot
CI_Allegations_BP <- function(plotName){
  tab <- tabPanel('Allegations Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Checkbox group"), 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}


# Makes the tab for call source barplot
CI_Involvement_BP <- function(plotName){
  tab <- tabPanel('Involvement Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Checkbox group"), 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}

# Makes the tab for subject's age barplot
CI_Age_BP <- function(plotName){
  tab <- tabPanel('Age Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Checkbox group"), 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}




# Makes the tab for call source barplot
CI_Subject_Type_BP <- function(plotName){
  tab <- tabPanel('Subject Type Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Checkbox group"), 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1))
  return(tab)
}


# Temporary render function TODO: Work out how data team wants to pass in
CI_render <- function(output, plot1, plot2, plot3, plot4, plot5, plot6, plot7){
  output$CI_table_1 <- plot1
  output$CI_table_2 <- plot2
  output$CI_table_3 <- plot3
  output$CI_table_4 <- plot4
  output$CI_table_5 <- plot5
  output$CI_table_6 <- plot6
  output$CI_table_7 <- plot7
}
