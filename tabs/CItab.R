##################################################################
##################################################################
##             File for the Use of force UI                     ##
##################################################################
##################################################################
# This file is for UI stuff related to Use of force Tab

## TODO: make this file do something

# Function that handles the large scale formatting of the main area (dashboardBody) and the tabs on top
CI_tab <- function(){
  data <- read.csv(file("joined-data.csv"))
  
  # Replace UOF values with NA so that UOF data is filtered out
  data$RACE <- replace(data$RACE, data$INCIDENT_TYPE == "Use of force", NA)
  
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
  
  # Replace UOF values with NA so that UOF data is filtered out
  data$SEX <- replace(data$SEX, data$INCIDENT_TYPE == "Use of force", NA)
  
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
  
  # Replace UOF values with NA so that UOF data is filtered out
  data$YRS_EMPL <- replace(data$YRS_EMPL, data$INCIDENT_TYPE == "Use of force", NA)
  
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
  
  # Replace UOF values with NA so that UOF data is filtered out
  data$INVOLVMENT <- replace(data$INVOLVMENT, data$INCIDENT_TYPE == "Use of force", NA)
  
  # Replace UOF values with NA so that UOF data is filtered out
  data$AGE <- replace(data$AGE, data$INCIDENT_TYPE == "Use of force", NA)
  
  # Replace UOF values with NA so that UOF data is filtered out
  data$SUBJ_TYPE <- replace(data$SUBJ_TYPE, data$INCIDENT_TYPE == "Use of force", NA)
  
  # Fix Data -  Labels are too long, need to reduce number of characters
  data$ALLEGATION_MADE <- sapply(data$ALLEGATION_MADE, function(x) {
    if (is.na(x) || x == "Discrimination, Oppression or Favoritism - Color") # "Color" by itself is a strange allegation
    { 
      return(x)
    } else {
      return(strsplit(as.character(x), split = " - ")[[1]][2]) # Splits into before and after '-', using elements in [2]
    }
  })
  
  # Makes the object of the entire main area
  tab <- tabItem(tabName = "CI",
                 tabBox(
                   height = "500px",
                   CI_Race_PC(data, "CI_table_1", "CI_Race_Selector"),
                   CI_Sex_PC(data, "CI_table_2", "CI_Sex_Selector"),
                   CI_Years_Employed_BP(data, "CI_table_3", "CI_Years_Employed_Selector")
                 ),
                 tabBox(
                   height = "500px",
                   CI_Allegations_BP(data, "CI_table_4", "CI_Allegations_Selector"),
                   CI_Involvement_BP(data, "CI_table_5", "CI_Involvement_Selector"),
                   CI_Age_BP(data, "CI_table_6", "CI_Age_Selector"),
                   CI_Subject_Type_BP(data, "CI_table_7", "CI_Subject_Type_Selector")
                 )
  )
  return(tab)
}

# --- Pie Charts ---

# Makes the tab for race piechart
CI_Race_PC <- function(data, plotName, widgetName){
  tab <- tabPanel('Race Piechart', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Selector", c("Unselected", unique(data$RACE[!is.na(data$RACE)])), selected = 1)
                  )
  return(tab)
}

# Makes the tab for sex piechart
CI_Sex_PC <- function(data, plotName, widgetName){
  tab <- tabPanel('Sex Piechart', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Selector", c("Unselected", unique(data$SEX[!is.na(data$SEX)])), selected = 1)
                  )
  return(tab)
}


# --- Bar Charts ---

# Makes the tab for subject's years employed barplot

CI_Years_Employed_BP <- function(data, plotName, widgetName){
  tab <- tabPanel('Years Employed Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Selector", c("Unselected", unique(data$YRS_EMPL[!is.na(data$YRS_EMPL)])), selected = 1)
                  )
  return(tab)
}


# Makes the tab for allegations barplot
CI_Allegations_BP <- function(data, plotName, widgetName){
  tab <- tabPanel('Allegations Bargraph', # Tab title
                  plotOutput(plotName), # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Selector", c("Unselected", unique(data$ALLEGATION_MADE[!is.na(data$ALLEGATION_MADE)])), selected = 1)
                  )
  return(tab)
}


# Makes the tab for call source barplot
CI_Involvement_BP <- function(data, plotName, widgetName){
  tab <- tabPanel('Involvement Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Selector", c("Unselected", unique(data$INVOLVMENT[!is.na(data$INVOLVMENT)])), selected = 1)
                  )
  return(tab)
}

# Makes the tab for subject's age barplot
CI_Age_BP <- function(data, plotName, widgetName){
  tab <- tabPanel('Age Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Selector", c("Unselected", unique(data$AGE[!is.na(data$AGE)])), selected = 1)
                  )
}




# Makes the tab for call source barplot
CI_Subject_Type_BP <- function(data, plotName, widgetName){
  tab <- tabPanel('Subject Type Bargraph', # Tab title
                  plotOutput(plotName),                 # plotOutput name
                  # Input selector
                  selectInput(widgetName, "Selector", c("Unselected", unique(data$SUBJ_TYPE[!is.na(data$SUBJ_TYPE)])), selected = 1)
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
