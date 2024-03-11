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
        UOF_Sex_PC("UOF_table_2"),
        UOF_Years_Employed_BP("UOF_table_3")
      ),
     tabBox(
       height = "500px",
       UOF_Involvement_BP("UOF_table_4"),
       UOF_Age_BP("UOF_table_5"),
       UOF_Subject_Type_BP("UOF_table_6")
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
      label = h3("Display Race"), 
      choices = list("Asain" = 1, "Black" = 2, "Filipino" = 3, "Hispanic" = 4, "Mixed" = 5, "Native Am" = 6, "Unknown" = 7, "White" = 8)))
  return(tab)
}

# Makes the tab for sex piechart
UOF_Sex_PC <- function(plotName){
  tab <- tabPanel('Sex Piechart', # Tab title
                  plotOutput(plotName))                 # plotOutput name
  return(tab)
}

# Makes the tab for years employed
UOF_Years_Employed_BP <- function(plotName){
  tab <- tabPanel('Years Employed Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Years Employed Display"), 
                                     choices = list("0-10" = 1, "11-20" = 2)))
  return(tab)
}


# Makes the tab for involvement barplot
UOF_Involvement_BP <- function(plotName){
  tab <- tabPanel('Involvement Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Involvement Display"), 
                                     choices = list("Complainant" = 1, "Officer" = 2, "Witness" = 3)))
  return(tab)
}

# Makes the tab for age barplot
UOF_Age_BP <- function(plotName){
  tab <- tabPanel('Age Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  sliderInput("slider", label = "Select Age Range", min=0, max=100, value = 0))
  return(tab)
}




# Makes the tab for call source barplot
UOF_Subject_Type_BP <- function(plotName){
  tab <- tabPanel('Subject Type Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Subject Type Display"), 
                                     choices = list("Citizen" = 1, "Officer" = 2, "Officer/Reporter" = 3, "Officer/Witness" = 4)))
  return(tab)
}


# Temporary render function TODO: Work out how data team wants to pass in
UOF_render <- function(output, plot1, plot2, plot3, plot4, plot5, plot6){
  output$UOF_table_1 <- plot1
  output$UOF_table_2 <- plot2
  output$UOF_table_3 <- plot3
  output$UOF_table_4 <- plot4
  output$UOF_table_5 <- plot5
  output$UOF_table_6 <- plot6
}