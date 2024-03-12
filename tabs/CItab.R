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
                   CI_Sex_PC("CI_table_2"),
                   CI_Years_Employed_BP("CI_table_3")
                 ),
                 tabBox(
                   height = "500px",
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
                                     label = h3("Display Race"), 
                                     choices = list("Amer Ind" = 1, "Asain" = 2, "Black" = 3, "Hispanic" = 4, "Unknown" = 5, "Vietnamese" = 6, "White" = 7)),
                  # Button
                  actionButton("select", 
                               "Button"),
                  
                  # Date range
                  dateRangeInput("daterange", 
                                 "Date Range"),
                  
                  # Multiple button options to select from
                  radioButtons("radioButton", 
                               "Buttons", 
                               choices = list("Option1" = 1, "Option2" = 2))
                  )
  return(tab)
}

# Makes the tab for sex piechart
CI_Sex_PC <- function(plotName){
  tab <- tabPanel('Sex Piechart', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Display Gender"), 
                                     choices = list("Female" = 1, "Male" = 2, "Unknown" = 3)),
                  # Button
                  actionButton("select", 
                               "Button"),
                  
                  # Date range
                  dateRangeInput("daterange", 
                                 "Date Range"),
                  
                  # Multiple button options to select from
                  radioButtons("radioButton", 
                               "Buttons", 
                               choices = list("Option1" = 1, "Option2" = 2))
                  )
  return(tab)
}


# --- Bar Charts ---

# Makes the tab for subject's years employed barplot

CI_Years_Employed_BP <- function(plotName){
  tab <- tabPanel('Years Employed Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Display Years Employed"), 
                                     choices = list("0-10" = 1, "11-20" = 2, "21-30" = 3, "31-40" = 3)),
                  # Button
                  actionButton("select", 
                               "Button"),
                  
                  # Date range
                  dateRangeInput("daterange", 
                                 "Date Range"),
                  
                  # Multiple button options to select from
                  radioButtons("radioButton", 
                               "Buttons", 
                               choices = list("Option1" = 1, "Option2" = 2))
                  )
  return(tab)
}


# Makes the tab for allegations barplot
CI_Allegations_BP <- function(plotName){
  tab <- tabPanel('Allegations Bargraph', # Tab title
                  plotOutput(plotName), # plotOutput name
                  
                  # Button
                  actionButton("select", 
                               "Button"),
                  
                  # Date range
                  dateRangeInput("daterange", 
                                 "Date Range"),
                  
                  # Multiple button options to select from
                  radioButtons("radioButton", 
                               "Buttons", 
                               choices = list("Option1" = 1, "Option2" = 2))
                  )
  return(tab)
}


# Makes the tab for call source barplot
CI_Involvement_BP <- function(plotName){
  tab <- tabPanel('Involvement Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Display Involvement"), 
                                     choices = list("Complainant" = 1, "Officer" = 2, "Choice 3" = 3)),
                  # Button
                  actionButton("select", 
                               "Button"),
                  
                  # Date range
                  dateRangeInput("daterange", 
                                 "Date Range"),
                  
                  # Multiple button options to select from
                  radioButtons("radioButton", 
                               "Buttons", 
                               choices = list("Option1" = 1, "Option2" = 2))
                  )
  return(tab)
}

# Makes the tab for subject's age barplot
CI_Age_BP <- function(plotName){
  tab <- tabPanel('Age Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  sliderInput("slider", label = "Select Age Range", min=0, max=100, value = 0),
                  # Button
                  actionButton("select", 
                               "Button"),
                  
                  # Date range
                  dateRangeInput("daterange", 
                                 "Date Range"),
                  
                  # Multiple button options to select from
                  radioButtons("radioButton", 
                               "Buttons", 
                               choices = list("Option1" = 1, "Option2" = 2))
                  )
}




# Makes the tab for call source barplot
CI_Subject_Type_BP <- function(plotName){
  tab <- tabPanel('Subject Type Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Graph controls
                  checkboxGroupInput("checkGroup", 
                                     label = h3("Display Subject Type"), 
                                     choices = list("Citizen" = 1, "Officer" = 2)),
                  # Button
                  actionButton("select", 
                               "Button"),
                  
                  # Date range
                  dateRangeInput("daterange", 
                                 "Date Range"),
                  
                  # Multiple button options to select from
                  radioButtons("radioButton", 
                               "Buttons", 
                               choices = list("Option1" = 1, "Option2" = 2))
                  )
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
