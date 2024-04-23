##################################################################
##################################################################
##             File for the Use of force UI                     ##
##################################################################
##################################################################
# This file is for UI stuff related to Use of force Tab

## TODO: make this file do something

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
UOF_tab <- function(){
  data <- read.csv(file("joined-data.csv"))
  
  # Replace CI values with NA so that CI data is filtered out
  data$RACE <- replace(data$RACE, grepl("Complaint", data$INCIDENT_TYPE), NA)
  
  # Fix Data - Typos
  data$RACE <- sapply(data$RACE, function(x) {
    
    if (grepl("Unkn", x)) # Unknown accounted for
    {
      return(x)
    }
    else if(grepl("N/A", x)) # N/A accounted for
    {
      return(NA)
    }
    else if(grepl("black", x)) # black accounted for
    { 
      return("Black")
    }  
    
    else if (grepl("w", x) ) # white, w accounted for
    { 
      return("White")
    } 
    else  # Amer Ind, Asian, Black, Hispanic, "Vietnamese", "White" is accounted for
    {
      return(x) 
    }
  })
  
  # Replace CI values with NA so that CI data is filtered out
  data$SEX <- replace(data$SEX, grepl("Complaint", data$INCIDENT_TYPE), NA)
  
  # Fix Data - Typos
  data$SEX <- sapply(data$SEX, function(x) {
    
    if (grepl("Unkn", x)) # Unknown accounted for
    {
      return(x)
    }
    else if (grepl("emale", x)) # Female and female accounted for
    { 
      return("Female")
    }  
    else if(!grepl("Male", x)) # male, m accounted for
    {
      return("Male")
    }
    else  # NA and Male accounted for
    {
      return(x) 
    }
    
  })
  
  # Replace CI values with NA so that CI data is filtered out
  data$YRS_EMPL <- replace(data$YRS_EMPL, grepl("Complaint", data$INCIDENT_TYPE), NA)
  
  # Fix Data - Too many values, sort them into ranges
  data$YRS_EMPL <- sapply(data$YRS_EMPL, function(x) 
  {
    if(is.na(x))
    {
      return(NA)
    }
    else if (x < 11) # Unknown accounted for
    {
      return("0-10")
    }
    else if (11 <= x && x <= 20) # Female and female accounted for
    { 
      return("11-20")
    }  
    else if(21 <= x && x <= 30) # male, m accounted for
    {
      return("21-30")
    }
    else if(31 <= x && x <= 40)
    {
      return("31-40") 
    }
    else if(40 <= x && x<= 50)
    {
      return("41-50") 
    }
  })
  
  # Replace CI values with NA so that CI data is filtered out
  data$INVOLVMENT <- replace(data$INVOLVMENT, grepl("Complaint", data$INCIDENT_TYPE), NA)
  
  # Replace UOF values with NA so that UOF data is filtered out
  data$AGE <- replace(data$AGE, grepl("Complaint", data$INCIDENT_TYPE), NA)
  
  # Replace CI values with NA so that CI data is filtered out
  data$SUBJ_TYPE <- replace(data$SUBJ_TYPE, grepl("Complaint", data$INCIDENT_TYPE), NA)
  
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "UOF",
                 tabBox(
                   height = "500px",
                   UOF_Race_PC(data, "UOF_table_1", "UOF_Race_Selector"),
                   UOF_Sex_PC(data, "UOF_table_2", "UOF_Sex_Selector"),
                   UOF_Years_Employed_BP(data, "UOF_table_3", "UOF_Years_Employed_Selector")
                 ),
                 tabBox(
                   height = "500px",
                   UOF_Involvement_BP(data, "UOF_table_4", "UOF_Involvement_Selector"),
                   UOF_Age_BP(data, "UOF_table_5", "UOF_Age_Selector"),
                   UOF_Subject_Type_BP(data, "UOF_table_6", "UOF_Subject_Type_Selector")
                 )
  )
  return(tab)
}

# Makes the tab for race piechart
UOF_Race_PC <- function(data, plotName, widgetName){
  tab <- tabPanel('Race Piechart', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Race-Selector", c("Unselected", unique(data$RACE[!is.na(data$RACE)])), selected = 1)
  )
  return(tab)
}

# Makes the tab for sex piechart
UOF_Sex_PC <- function(data, plotName, widgetName){
  tab <- tabPanel('Sex Piechart', # Tab title
                  plotOutput(plotName), # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Sex-Selector", c("Unselected", unique(data$SEX[!is.na(data$SEX)])), selected = 1)
  )
  return(tab)
}

# Makes the tab for years employed
UOF_Years_Employed_BP <- function(data, plotName, widgetName){
  tab <- tabPanel('Years Employed Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Years Employed-Selector", c("Unselected", unique(data$YRS_EMPL[!is.na(data$YRS_EMPL)])), selected = 1)
  )
  return(tab)
}


# Makes the tab for involvement barplot
UOF_Involvement_BP <- function(data, plotName, widgetName){
  tab <- tabPanel('Involvement Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Involvement-Selector", c("Unselected", unique(data$INVOLVMENT[!is.na(data$INVOLVMENT)])), selected = 1)
  )
  return(tab)
}

# Makes the tab for age barplot
UOF_Age_BP <- function(data, plotName, widgetName){
  tab <- tabPanel('Age Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Age-Selector", c("Unselected", unique(data$AGE[!is.na(data$AGE)])), selected = 1)
  )
  return(tab)
}

# Makes the tab for call source barplot
UOF_Subject_Type_BP <- function(data, plotName, widgetName){
  tab <- tabPanel('Subject Type Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Subject Type-Selector", c("Unselected", unique(data$SUBJ_TYPE[!is.na(data$SUBJ_TYPE)])), selected = 1)
  )
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