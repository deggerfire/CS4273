library(shiny)          # The sever thingy
library(ggplot2)        # Used for plotting
library(dplyr)          # Used for data handling
library(shinydashboard) # Used for fancy UI stuff
library(stringr)        #Used for subsetting the data into strings

# Stuff for running a server
options(shiny.host = '0.0.0.0') # IP-address of computer
options(shiny.port = 5111) # Port you want to host on

DEBUG <<- FALSE
# Import the tab files

# test

source("tabs/CFStab.R")
source("tabs/COL1tab.R")
source("tabs/COL2tab.R")
source("tabs/COL3tab.R")
source("tabs/COL4tab.R")
source("tabs/UOFtab.R")
source("tabs/CItab.R")
source("tabs/CONtab.R")
source("tabs/OFF1tab.R")
source("tabs/OFF2tab.R")
source("tabs/OFF3tab.R")
source("tabs/OFF4tab.R")
ui <- dashboardPage(
  ######################################################################
  ######################################################################
  #######                      Main page UI                     ########
  ####### UI people should be the only ones making changes here ########
  ######################################################################
  ######################################################################
  
  # Sets the title and also adds logo
  dashboardHeader(title='Norman PD', titleWidth = 275,
                  tags$li(class = "dropdown", imageOutput("logo", height = 25))),
  
  # Left sidebar, used to to get to major categories
  dashboardSidebar(
    width = 275,
    sidebarMenu(
      # Variable name of this sidebar
      id = "sidebar",
      #             name on the sidebar for user                  var name in code      icon on screen
      menuItem("Calls for Service"                               , tabName = "CFS", icon = icon("phone")),
      menuItem("Collisions"                                      , tabName = "COL", icon = icon("car-burst"),
               menuSubItem('By Severity'                         , tabName = 'COL1', icon = icon('triangle-exclamation')),
               menuSubItem('By injury'                           , tabName = 'COL2', icon = icon('user-injured')),
               menuSubItem('By Location'                           , tabName = 'COL3', icon = icon('location-dot')),
               menuSubItem('Throughout Year'                           , tabName = 'COL4', icon = icon('calendar-days'))),
      
      menuItem("Use of Force"                                    , tabName = "UOF", icon = icon("hand-fist")),
      #menuSubItem('Subjects by Resistance and Force'            , tabName = 'UOF4')),
      #menuSubItem('Subjects by Incidents and Demographics'      , tabName = 'UOF5')),
      menuItem("Complaints and Inquiries"                        , tabName = "CI", icon = icon("bullhorn")),
      #menuSubItem('Incidents by Type and Disposition'           , tabName = 'CI1'),
      #menuSubItem('Subjects by Incidents and Demographics'      , tabName = 'CI2'),
      #menuSubItem('Subjects by Allegation and Finding'          , tabName = 'CI3'),
      
      menuItem("Contacts"                                        , tabName = "CON", icon = icon("people-arrows")),
      #menuSubItem('Traffic and Parking Contacts'                , tabName = 'CON1')),
      menuItem("Offenses"                                        , tabName = "OFF", icon = icon("gavel"),
               menuSubItem('Case Offenses'                       , tabName = 'OFF1', icon = icon("bell")),
               menuSubItem('Case Details'                        , tabName = 'OFF2', icon = icon("circle-info")),
               menuSubItem('Subjects'                            , tabName = 'OFF3', icon = icon("circle-user")),
               menuSubItem('Arrests'                             , tabName = 'OFF4', icon = icon("handcuffs")))
      
    )
  ),
  
  # Main body where graphs are rendered (they are all in their own files)
  dashboardBody(id = "tabs",
                tabItems(
                  CFS_tab(), # Calls for service tab
                  COL1_tab(),# Collision Tab, By Severity
                  COL2_tab(),# Collision Tab, By injury
                  COL3_tab(),# Collision Tab, By Location
                  COL4_tab(),# Collision Tab, Throughout Year
                  UOF_tab(), # Use of force Tab
                  CI_tab(),  # Complaints and Inquiries Tab
                  CON_tab(), # Contacts Tab
                  OFF1_tab(),# Offense Tab, case offenses
                  OFF2_tab(),# Offense Tab, case details
                  OFF3_tab(),# Offense Tab, subjects
                  OFF4_tab() # Offense Tab, arrests
                ),
                error = div("An error occurred while rendering the tab. Please try again later.")
  )
)

# This is where we will define server logic. Ie, this is where we will parse the CSV,
# add graphs, create sliders/filters for user input, ect

# The bulk of our work will be here. Most of the time you will be 
# working in your teams trigger method

server <- function(input, output, session) {
  
  # Renders logo
  output$logo <- renderImage({
    list(src = "norman_pd_logo.jpeg", height = 50)
  }, deleteFile = FALSE)
  
  ######################################################################
  ######################################################################
  #######################  Graph making methods ########################
  ######################################################################
  ######################################################################
  # Makes a barplot object using the inputted data
  # This function is for step 3
  # data - the data that is to be rendered, must be tabled
  # label - string for graph labels
  outputBarPlot <- function(data, label = "") {
    plot <- renderPlot({
      tryCatch({
        # Put the plot at plotOutput("Barplot") in the shiny code
        graph <- ggplot(data.frame(data), aes(x = Var1, y = Freq, fill = Var1)) +
          geom_bar(stat = "identity", width = 0.8) +
          geom_text(aes(label = Freq), vjust = -0.5, size = 4) +  # Add numbers to bars
          labs(x = label, y = "Amount", fill = label) +
          theme_minimal() +
          theme(
            text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            legend.position = "none"
          )
        print(graph)
      }, error = function(e) {graph <- ggplot() + annotate("text", x = 0, y = 0, size = 10, label = "No Data to Graph") + theme_void()
      print(graph)})
    })
    return(plot)
  }
  
  outputSpecialBarPlot <- function(data, label = "") {
    plot <- renderPlot({
      tryCatch({
        # Put the plot at plotOutput("Barplot") in the shiny code
        graph <- ggplot(data.frame(data), aes(x = Var1, y = Freq, fill = Var1)) +
          geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
          geom_text(aes(label = Freq), vjust = -0.5, size = 4) +  # Add numbers to bars
          labs(x = label, y = "Amount", fill = label) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0),
            text = element_text(size = 14)
          )
        print(graph)
      }, error = function(e) {graph <- ggplot() + annotate("text", x = 0, y = 0, size = 10, label = "No Data to Graph") + theme_void()
      print(graph)})
    })
    return(plot)
  }
  
  outputPieChart <- function(data, label = "") {
    
    plot <- renderPlot({
      tryCatch({
        # Put the plot at plotOutput("Piechart") in the shiny code
        # Calculates percentages
        percent <- (data/sum(data))*100
        percent <- round(percent, digits=2)
        graph <- ggplot(data.frame(data), aes(x = "", y = Freq, fill = Var1)) +
          geom_bar(stat = "identity", width = 1) +
          geom_text(aes(label = paste0(round(Freq/sum(Freq)*100), "%")), position = position_stack(vjust = 0.5), size = 4, check_overlap = TRUE) +  # Add percentages to pie chart
          labs(fill = label) +
          theme_void() +
          theme(
            text = element_text(size = 14),
            legend.position = "right"
          ) +
          coord_polar("y", start = 0) +
          scale_fill_brewer(palette = "Set3", labels=paste(data.frame(data)$Var1, " (", percent, "%)", sep=""))
        print(graph)
      }, error = function(e) {graph <- ggplot() + annotate("text", x = 0, y = 0, size = 10, label = "No Data to Graph") + theme_void()
      print(graph)})
    })
    return(plot)
  }
  
  outputLineGraph <- function(data, x, y, label = "", xlab = "", ylab = "") {
    plot <- renderPlot({
      tryCatch({
        # Put the plot at plotOutput("Piechart") in the shiny code
        graph <- ggplot(data, aes_string(x = x, y = y, group = 1)) +
          geom_line(colour = 'red') +
          xlab(xlab) +
          ylab(ylab) +
          ggtitle(label) +
          theme(
            text = element_text(size = 14)
          )
        print(graph)
      }, error = function(e) {graph <- ggplot() + annotate("text", x = 0, y = 0, size = 10, label = "No Data to Graph") + theme_void()
      print(graph)})
    })
    return(plot)
  }
  findNumOfYears <- function(data, CFSCOL, CON, OFF, OFF23, OFF4)
  {
    #For all data sets except contacts
    if(CFSCOL == TRUE)
    {
      numOfYear <- c()
      first = head(data, 1)
      last = tail(data, 1)
      first = first %>% select(contains("Date")) %>% str_sub(-2, -1)
      last = last %>%select(contains("Date")) %>% str_sub(-2, -1)
      x = as.numeric(first)
      numOfYear <- append(numOfYear, "All Years")
      numOfYear <- append(numOfYear,paste0("20",as.character(x)))
      last = as.numeric(last)
      while(x <= last)
      {
        numOfYear <- append(numOfYear,paste0("20",as.character(x)))
        x = x + 1
      }
      sort(numOfYear, decreasing = FALSE)
      numOfYear
      return(numOfYear)
    }
    #For the contacts dataset (This dataset shows times as well as date, and must be filtered differently)
    else if(CON == TRUE)
    {
      numOfYear <- c()
      first = head(data, 1)
      last = tail(data, 1)
      first = first %>% select(contains("Date")) %>% str_sub(-17, -16)
      last = last %>%select(contains("Date")) %>% str_sub(-17, -16)
      x = as.numeric(first)
      numOfYear <- append(numOfYear, "All Years")
      numoOfYear = append(numOfYear,paste0("20",as.character(x)))
      last = as.numeric(last)
      while(x <= last)
      {
        numOfYear <- append(numOfYear,paste0("20",as.character(x)))
        x = x + 1
      }
      sort(numOfYear, decreasing = FALSE)
      return(numOfYear)
    }
    else if(OFF == TRUE)
    {
     
      if(OFF23 == TRUE)
      {
        df <- data.frame(data)
        #Sorted by years through CaseNumber Column
        numOfYear <- c()
        first = head(data, 1)
        last =  df[18779,]  #tail(data, 1) Normally used but there was empty hidden characters in the CSV #Change this row when data is fixed #########
        first = first %>% select(contains("CaseN")) %>% str_sub(-11, -10)
        last = last %>%select(contains("CaseN")) %>% str_sub(-11, -10)
        x = as.numeric(first)
        numOfYear <- append(numOfYear, "All Years")
        numoOfYear = append(numOfYear,paste0("20",as.character(x)))
        last = as.numeric(last)
        while(x <= last)
        {
          numOfYear <- append(numOfYear,paste0("20",as.character(x)))
          x = x + 1
        }
        sort(numOfYear, decreasing = FALSE)
        return(numOfYear)
      }
      else if(OFF4 == TRUE)
      {
        df <- data.frame(data)
        #Sorted by years through CaseNumber Column
        numOfYear <- c()
        first = head(data, 1)
        last = tail(data, 1) #Normally used but there was empty hidden characters in the CSV #Change this row when data is fixed 
        first = first %>% select(contains("CaseN")) %>% str_sub(-11, -10)
        last = last %>%select(contains("CaseN")) %>% str_sub(-11, -10)
        x = as.numeric(first)
        numOfYear <- append(numOfYear, "All Years")
        numoOfYear = append(numOfYear,paste0("20",as.character(x)))
        last = as.numeric(last)
        while(x <= last)
        {
          numOfYear <-append(numOfYear,paste0("20",as.character(x)))
          x = x + 1
        }
        sort(numOfYear, decreasing = FALSE)
        return(numOfYear)
      }
      else
      {
        #This is for OFF1
        df <- data.frame(data)
        #Sorted by years through CaseNumber Column
        numOfYear <- c()
        first = head(data, 1)
        last =  df[15656,]  #tail(data, 1) Normally used but there was empty hidden characters in the CSV #Change this row when data is fixed 
        first = first %>% select(contains("CaseN")) %>% str_sub(-11, -10)
        last = last %>%select(contains("CaseN")) %>% str_sub(-11, -10)
        x = as.numeric(first)
        numOfYear <- append(numOfYear, "All Years")
        numoOfYear = append(numOfYear,paste0("20",as.character(x)))
        last = as.numeric(last)
        while(x <= last)
        {
          numOfYear <- append(numOfYear,paste0("20",as.character(x)))
          x = x + 1
        }
        sort(numOfYear, decreasing = FALSE)
        return(numOfYear)
      }
    }
  }
  
  
  
  #Used to get the number of accidents per week for throughout year
  getAccidentsPerWeek <- function(data, bool){
    #Code to get the number of accidents per month
    
    #Needs if statement to change where substring starts/ends to compare in for loop
    if(bool == TRUE){ a = 4; b = 5}
    else{a = 3; b = 4}
    
    week1 = 0
    week2 = 0
    week3 = 0
    week4 = 0
    week5 = 0
    for (i in 1:length(data)){
      x = substr(data[i], a, b)
      if(x == "1/" | x == "2/" | x == "3/" | x == "4/" | x == "5/" | x == "6/" | x == "7/")
        week1 = week1 + 1
      else if(x == "8/" | x == "9/" | x == "10" | x == "11" | x == "12" | x == "13" | x == "14")
        week2 = week2 + 1
      else if(x == "15" | x == "16" | x == "17" | x == "18" | x == "19" | x == "20" | x == "21")
        week3 = week3 + 1
      else if(x == "22" | x == "23" | x == "24" | x == "25" | x == "26" | x == "27" | x == "28")
        week4 = week4 + 1
      else if(x == "29" | x == "30" | x == "31")
        week5 = week5 + 1
    }
    weeks = c(week1, week2, week3, week4, week5)
    return(weeks)
  }
  
  
  
  ######################################################################
  ######################################################################
  ######## observeEvent/trigger (react to user doing something) ########
  ######################################################################
  ######################################################################
  
  # List that triggers observeEvent() whenever anything on the screen is changed
  toObserve <- reactive({
    reactiveValuesToList(input)
  })
  
  # Method that gets triggered when the graph is suppose to change or update
  observeEvent(toObserve(), {
    # Call both group A's and group L's trigger function
    groupAtrigger()
    groupLtrigger()
  })
  
  ######################################################################
  ###################  Group A's trigger method ########################
  ######################################################################
  # Groups A's method that gets triggered when the graph is suppose to change
  groupAtrigger <- function(){
    # If chain that checks for what type of graph is selected
    #(R's switch would not work here)
    if(input$sidebar == "UOF")
    {
      data <- read.csv(file("joined-data.csv"))
      
      # PIE CHARTS 
      
      # 1. Race
      
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
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$UOF_Race_Selector != "Unselected"){
        data <- data %>% filter(RACE == input$UOF_Race_Selector)
      }
      
      # Final Output
      race <- outputPieChart(table(data$RACE), label = "Race")
      
      # 2. Sex
      
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
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$UOF_Sex_Selector != "Unselected"){
        data <- data %>% filter(SEX == input$UOF_Sex_Selector)
      }
      
      # Final Output
      sex <- outputPieChart(table(data$SEX), label = "Gender")
      
      # BAR CHARTS
      
      # 3. Years Employed
      
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
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$UOF_Years_Employed_Selector != "Unselected"){
        data <- data %>% filter(YRS_EMPL == input$UOF_Years_Employed_Selector)
      }
      
      # Final Output
      years_employed <- outputBarPlot(table(data$YRS_EMPL), label = "Years Employed")
      
      # 4. Involvement
      
      # Replace CI values with NA so that CI data is filtered out
      data$INVOLVMENT <- replace(data$INVOLVMENT, grepl("Complaint", data$INCIDENT_TYPE), NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$UOF_Involvement_Selector != "Unselected"){
        data <- data %>% filter(INVOLVMENT == input$UOF_Involvement_Selector)
      }
      
      #Final Output
      involvement <- outputBarPlot(table(data$INVOLVMENT), label = "Involvement")
      
      # 5. Age
      
      # Replace UOF values with NA so that UOF data is filtered out
      data$AGE <- replace(data$AGE, grepl("Complaint", data$INCIDENT_TYPE), NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$UOF_Age_Selector != "Unselected"){
        data <- data %>% filter(AGE == input$UOF_Age_Selector)
      }
      
      # Final Output
      age <- outputBarPlot(table(data$AGE), label = "Age")
      
      # 6. Subject Type
      
      # Replace CI values with NA so that CI data is filtered out
      data$SUBJ_TYPE <- replace(data$SUBJ_TYPE, grepl("Complaint", data$INCIDENT_TYPE), NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$UOF_Subject_Type_Selector != "Unselected"){
        data <- data %>% filter(SUBJ_TYPE == input$UOF_Subject_Type_Selector)
      }
      
      # Final Output
      subject_type <- outputBarPlot(table(data$SUBJ_TYPE), label = "Subject Type")
      
      # Render
      UOF_render(output, race, sex, years_employed, involvement, age, subject_type)
      
      output$UOF_table_1 <- race
      output$UOF_table_2 <- sex
      output$UOF_table_3 <- years_employed
      output$UOF_table_4 <- involvement
      output$UOF_table_5 <- age
      output$UOF_table_6 <- subject_type
    }
    else if (input$sidebar == "CI") 
    {
      data <- read.csv(file("joined-data.csv"))
      
      # PIE CHARTS
      
      # 1. Race 
      
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
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$CI_Race_Selector != "Unselected"){
        data <- data %>% filter(RACE == input$CI_Race_Selector)
      }
      
      # Final Output
      race <- outputPieChart(table(data$RACE), label = "Race")
      
      # 2. Sex
      
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
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$CI_Sex_Selector != "Unselected"){
        data <- data %>% filter(SEX == input$CI_Sex_Selector)
      }
      
      # Final Output
      sex <- outputPieChart(table(data$SEX), label = "Gender")
      
      # BAR CHARTS
      
      # 3. Years Employed
      
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
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$CI_Years_Employed_Selector != "Unselected"){
        data <- data %>% filter(YRS_EMPL == input$CI_Years_Employed_Selector)
      }
      
      # Final Output
      years_employed <- outputBarPlot(table(data$YRS_EMPL), label = "Years Employed")
      
      # 4. Allegations Made 
      
      # No Allegations in UOF, no need to filter
      
      # Fix Data -  Labels are too long, need to reduce number of characters
      data$ALLEGATION_MADE <- sapply(data$ALLEGATION_MADE, function(x) {
        if (is.na(x) || x == "Discrimination, Oppression or Favoritism - Color") # "Color" by itself is a strange allegation
        { 
          return(x)
        } else {
          return(strsplit(as.character(x), split = " - ")[[1]][2]) # Splits into before and after '-', using elements in [2]
        }
      })
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$CI_Allegations_Selector != "Unselected"){
        data <- data %>% filter(ALLEGATION_MADE == input$CI_Allegations_Selector)
      }
      
      # Final Output (Special Bar Plot function used)
      allegations <- outputSpecialBarPlot(table(data$ALLEGATION_MADE), label = "Allegation")
      
      # 5. Involvement
      
      # Replace UOF values with NA so that UOF data is filtered out
      data$INVOLVMENT <- replace(data$INVOLVMENT, data$INCIDENT_TYPE == "Use of force", NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$CI_Involvement_Selector != "Unselected"){
        data <- data %>% filter(INVOLVMENT == input$CI_Involvement_Selector)
      }
      
      # Final Output
      involvement <- outputBarPlot(table(data$INVOLVMENT), label = "Involvement")
      
      # 6. Age
      
      # Replace UOF values with NA so that UOF data is filtered out
      data$AGE <- replace(data$AGE, data$INCIDENT_TYPE == "Use of force", NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$CI_Age_Selector != "Unselected"){
        data <- data %>% filter(AGE == input$CI_Age_Selector)
      }
      
      # Final Output
      age <- outputBarPlot(table(data$AGE), label = "Age")
      
      # 7. Subject Type
      
      # Replace UOF values with NA so that UOF data is filtered out
      data$SUBJ_TYPE <- replace(data$SUBJ_TYPE, data$INCIDENT_TYPE == "Use of force", NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$CI_Subject_Type_Selector != "Unselected"){
        data <- data %>% filter(SUBJ_TYPE == input$CI_Subject_Type_Selector)
      }
      
      # Final Output
      subject_type <- outputBarPlot(table(data$SUBJ_TYPE), label = "Subject Type")
      
      # Render
      CI_render(output, race, sex, years_employed, allegations, involvement, age, subject_type)
      
      output$CI_table_1 <- race
      output$CI_table_2 <- sex
      output$CI_table_3 <- years_employed
      output$CI_table_4 <- allegations
      output$CI_table_5 <- involvement
      output$CI_table_6 <- age
      output$CI_table_7 <- subject_type
    }
  }
  
  ######################################################################
  ###################  Group L's trigger method ########################
  ######################################################################
  # Groups L's method that gets triggered when the graph is suppose to change
  groupLtrigger <- function(){
    # If chain that checks for what type of graph is selected
    #(R's switch would not work here)
    if(input$sidebar == "CFS")
    {
      ######################
      # Step 1: read in the data
      ######################
      # Read in the call for service 2022
      data <- read.csv(file("CFS-2022.csv"))
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(data, TRUE, FALSE, FALSE, FALSE, FALSE)
      CFS_populateTopBar(session, numOfYears)
      
      if(input$CFSSelect_Year == "Unselected" || input$CFSSelect_Year == "All Years")
      {
        data <- data 
      }
      else
      {
        data <- data %>% filter(str_sub(CreateDatetime.UTC., -2, -1) == str_sub(input$CFSSelect_Year, -2, -1))
      }
      # Populate the widgets in CFS
      CFS_populate_Widgets(session, data$CallSource, data$PoliceCallStatus, data$PoliceCallPriority, data$City) #, data$PoliceCallType) This is the extra data for CFS tab
      ######################
      # Step 2: Filter the data
      ######################
      
      # If the user has selected an input for source of call then remove all that does not have the selected input
      
      if(input$CFS_Source_of_Call_Selector != "Unselected") {
        data <- data %>% filter(CallSource == input$CFS_Source_of_Call_Selector)
      }
      if(input$CFS_Police_Call_Status_Selector != "Unselected") {
        data <- data %>% filter(PoliceCallStatus == input$CFS_Police_Call_Status_Selector)
      }
      if(input$CFS_Police_Call_Priority_Selector != "Unselected") {
        data <- data %>% filter(PoliceCallPriority == input$CFS_Police_Call_Priority_Selector)
      }
      if(input$CFS_City_Selector != "Unselected") {
        data <- data %>% filter(City == input$CFS_City_Selector)
      }
      
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      # Makes the graph for source of call
      CS_BP   <- outputBarPlot (table(data$CallSource        ), label = "Source of Call")
      # Makes the graph for police call status
      PCS_PC  <- outputPieChart(table(data$PoliceCallStatus  ), label = "PoliceCallStatus")
      # Makes the graph for police call priority
      PCP_BP  <- outputBarPlot (table(data$PoliceCallPriority), label = "PoliceCallPriority")
      # Makes the graph for city
      City_PC <- outputPieChart(table(data$City              ), label = "City")
      
      ######################
      # Step 4: Put the graphs on screen
      ######################
      # Send the graphs off to the call for service render function to be put on screen
      CFS_render(output, CS_BP, PCS_PC, PCP_BP, City_PC)
      
    }
    else if(input$sidebar == "COL1") {
      ######################
      # Step 1: read in the data
      ######################
      data <- read.csv(file("By_Severity_2022.csv"))
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(data, TRUE, FALSE, FALSE, FALSE, FALSE)
      numOfYears
      COL1_populateTopBar(session, numOfYears)
      
      if(input$COL1Select_Year == "Unselected" || input$COL1Select_Year == "All Years")
      {
          data <- data
      }
      else
      {
          data <- data %>% filter(str_sub(AccidentDatetime, -2, -1) == str_sub(input$COL1Select_Year, -2, -1))
      }
      
      # Populate widgets for COL1 
      COL1_populate_Widgets(session, data$UnitType, data$DriverPedCondition, data$ChemicalTest, data$ContributingFactors)#data$UnitType) #Useless Data? Not being used currently ################################
      ######################
      # Step 2: Format the data
      ######################
      
      if(input$COL1_Selector_1 != "Unselected"){
        data <- data %>% filter(UnitType == input$COL1_Selector_1)
      }
      if(input$COL1_Selector_2 != "Unselected"){
        data <- data %>% filter(DriverPedCondition == input$COL1_Selector_2)
      }
      if(input$COL1_Selector_3 != "Unselected"){
        data <- data %>% filter(ChemicalTest == input$COL1_Selector_3)
      }
      if(input$COL1_Selector_4 != "Unselected"){
        data <- data %>% filter(ContributingFactors == input$COL1_Selector_4)
      }
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      # Makes the graph for source of call
      UnType_PC   <- outputPieChart (table(data$UnitType        ), label = "Unit Type")
      # Makes the graph for police call status
      ChemTest_PC  <- outputPieChart(table(data$ChemicalTest  ), label = "Chemical Test")
      # Makes the graph for police call priority
      DPC_BP  <- outputBarPlot (table(data$DriverPedCondition), label = "Driver Condition")
      # Makes the graph for city

      CF_freq <- data %>%
        group_by(ContributingFactors) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>%
        top_n(8, Count) %>%
        ungroup() %>%
        mutate(ContributingFactors = factor(ContributingFactors, levels = ContributingFactors))

      # Filter data to only include top 8 descriptions
      data_filtered <- data %>%
        filter(ContributingFactors %in% CF_freq$ContributingFactors)

      # Makes the graph for city with top 8 descriptions
      CF_BP <- outputBarPlot(table(data_filtered$ContributingFactors), label = "Contributing Factors")
      
      ######################
      # Step 4: Put the graphs on screen
      ######################
      COL1_render(output, UnType_PC, DPC_BP, ChemTest_PC, CF_BP)
    }
    else if(input$sidebar == "COL2")
    {
      ######################
      # Step 1: read in the data
      ######################
      data1 <- read.csv(file("pdicollisionsinjuriesq-2023.csv"))
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(data1, TRUE, FALSE, FALSE, FALSE, FALSE)
      COL2_populateTopBar(session, numOfYears)
      
      if(input$COL2Select_Year == "Unselected" || input$COL2Select_Year == "All Years")
      {
          data1 <- data1
      }
      else
      {
          data1 <- data1 %>% filter(str_sub(AccidentDatetime, -2, -1) == str_sub(input$COL2Select_Year, -2, -1))
      }
      #Populate widgets for COL2
      COL2_populate_Widgets(session, data1$Description, data1$Sev_Num)
      
      ######################
      # Step 2: Format the data
      ######################
      
      desc <- outputPieChart(table(data1$Description), label = "Outcome")
      severity <- outputBarPlot(table(data1$Sev_Num), label = "Severity Number")
      
      
      if(input$COL2_Selector_1 != "Unselected"){
        data1 <- data1 %>% filter(Description == input$COL2_Selector_1)
      }
      if(input$COL2_Selector_2 != "Unselected"){
        data1 <- data1 %>% filter(Sev_Num == input$COL2_Selector_2)
      }
      
      
      ######################
      # Step 3: Send the formatted data to become a graph
      #####################
      COL2_render(output, desc, severity)
    }
    else if(input$sidebar == "COL3")
    {
      ######################
      # Step 1: read in the data
      ######################
      data2 <- read.csv(file("pdicollisionsq-2023.csv"))
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(data2, TRUE, FALSE, FALSE, FALSE, FALSE)
      COL3_populateTopBar(session, numOfYears)

      if(input$COL3Select_Year == "Unselected" || input$COL3Select_Year == "All Years")
      {
        data2 <- data2
      }
      else
      {
        data2 <- data2 %>% filter(str_sub(AccidentDatetime, -2, -1) == str_sub(input$COL3Select_Year, -2, -1))
      }
      ######################
      # Step 2: Format the data
      ######################

      
      #Thirteen streets per dataSet, Each graph is by first letter in street. 
      streetAB = data2 %>% select(StreetType, StreetName) %>% filter(StreetType == "ST") %>% filter(substr(StreetName, 1, 1) == "A" 
                                                                                                    | substr(StreetName, 1, 1) == "B")
      
      streetCG = data2 %>% select(StreetType, StreetName) %>% filter(StreetType == "ST") %>% filter(substr(StreetName, 1, 1) == "C" 
                                                                                                    | substr(StreetName, 1, 1) == "D" 
                                                                                                    | substr(StreetName, 1, 1) == "E" 
                                                                                                    | substr(StreetName, 1, 1) == "F" 
                                                                                                    | substr(StreetName, 1, 1) == "G")
      
      streetHL = data2 %>% select(StreetType, StreetName) %>% filter(StreetType == "ST") %>% filter(substr(StreetName, 1, 1) == "H"
                                                                                                    | substr(StreetName, 1, 1) == "I"
                                                                                                    | substr(StreetName, 1, 1) == "J"
                                                                                                    | substr(StreetName, 1, 1) == "J"
                                                                                                    | substr(StreetName, 1, 1) == "K"
                                                                                                    | substr(StreetName, 1, 1) == "L")
      
      
      streetMZ = data2 %>% select(StreetType, StreetName) %>% filter(StreetType == "ST") %>% filter(substr(StreetName, 1, 1) == "M" 
                                                                                                    | substr(StreetName, 1, 1) == "N" 
                                                                                                    | substr(StreetName, 1, 1) == "O"
                                                                                                    | substr(StreetName, 1, 1) == "P" 
                                                                                                    | substr(StreetName, 1, 1) == "Q" 
                                                                                                    | substr(StreetName, 1, 1) == "R"
                                                                                                    | substr(StreetName, 1, 1) == "S" 
                                                                                                    | substr(StreetName, 1, 1) == "T" 
                                                                                                    | substr(StreetName, 1, 1) == "U"
                                                                                                    | substr(StreetName, 1, 1) == "V" 
                                                                                                    | substr(StreetName, 1, 1) == "W" 
                                                                                                    | substr(StreetName, 1, 1) == "X" 
                                                                                                    | substr(StreetName, 1, 1) == "Y" 
                                                                                                    | substr(StreetName, 1, 1) == "Z" )
      
      #Code to get the Main Street Graph
      mainStreets = data2 %>% select(StreetType, StreetName) %>% filter(StreetType == "ST") %>% filter(StreetName == "LINDSEY"
                                                                                                       | StreetName == "BOYD"
                                                                                                       | StreetName == "ALAMEDA"
                                                                                                       | StreetName == "GRAY"
                                                                                                       | StreetName == "MAIN"
                                                                                                       | StreetName == "ROBINSON")
      
      
    
      
      
      ######################
      # Step 3: Send the formatted data to become a graph
      #####################
      
      # Graphs for comparing number of accidents on streets through a bar chart
      SN_BP1 = outputSpecialBarPlot(table(streetAB$StreetName ), label = "Streets A-B")
      SN_BP2 = outputSpecialBarPlot(table(streetCG$StreetName ), label = "Streets C-G")
      SN_BP3 = outputSpecialBarPlot(table(streetHL$StreetName ), label = "Streets H-L")
      SN_BP4 = outputSpecialBarPlot(table(streetMZ$StreetName ), label = "Streets M-Z")
      
      # Graphs for comparing number of accidents on streets through a pie chart
      SN_PC1 = outputPieChart(table(streetAB$StreetName ), label = "Streets A-B")
      SN_PC2 = outputPieChart(table(streetCG$StreetName ), label = "Streets C-G")
      SN_PC3 = outputPieChart(table(streetHL$StreetName ), label = "Streets H-L")
      SN_PC4 = outputPieChart(table(streetMZ$StreetName ), label = "Streets M-Z")
      
      
      MS_BP1 = outputBarPlot(table(mainStreets$StreetName ), label = "Main Street Collisions 2024")
      MS_PC1 = outputPieChart(table(mainStreets$StreetName ), label = "Main Street Collisions 2024")
      
      
      COL3_render(output, SN_BP1, SN_BP2, SN_BP3, SN_BP4, SN_PC1, SN_PC2, SN_PC3, SN_PC4, MS_BP1, MS_PC1)
    }
    else if(input$sidebar == "COL4")
    {
      ######################
      # Step 1: read in the data
      ######################
      data2 <- read.csv(file("pdicollisionsq-2023.csv"))
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(data2, TRUE, FALSE, FALSE, FALSE, FALSE)
      COL4_populateTopBar(session, numOfYears)
      
      if(input$COL4Select_Year == "Unselected" || input$COL4Select_Year == "All Years")
      {
        data2 <- data2
      }
      else
      {
        data2 <- data2 %>% filter(str_sub(AccidentDatetime, -2, -1) == str_sub(input$COL4Select_Year, -2, -1))
      }
      
      ######################
      # Step 2: Format the data
      ######################
      
      
      #Code manipulating the data to get number of occurances per year       
      janCount = 0
      febCount = 0
      marCount = 0
      aprCount = 0
      mayCount = 0
      junCount = 0
      julCount = 0
      augCount = 0
      sepCount = 0
      octCount = 0
      novCount = 0
      decCount = 0
      for (i in 1:length(substr(data2$AccidentDatetime, 1, 2))){
        x = substr(data2$AccidentDatetime[i], 1, 2)
        if(x == "1/")
          janCount = janCount + 1
        else if(x == "2/")
          febCount = febCount + 1
        else if(x == "3/")
          marCount = marCount + 1
        else if(x == "4/")
          aprCount = aprCount + 1
        else if(x == "5/")
          mayCount = mayCount + 1
        else if(x == "6/")
          junCount = junCount + 1
        else if(x == "7/")
          julCount = julCount + 1
        else if(x == "8/")
          augCount = augCount + 1
        else if(x == "9/")
          sepCount = sepCount + 1
        else if(x == "10")
          octCount = octCount + 1
        else if(x == "11")
          novCount = novCount + 1
        else if(x == "12")
          decCount = decCount + 1
      }
      monthNumbers = c(janCount, febCount, marCount, aprCount, mayCount, junCount, julCount, augCount, sepCount, octCount, novCount, decCount)
      months = factor(c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), levels = month.abb)
      monthData = data.frame(monthNumbers, months)
      
    
      
      ############################
      #--------------------------#
      ############################
      
      #Week range used for all graphs showing collisions per month
      weekRange = c('1st-7th', '8th-14th', '15th-21nd', '22nd-28th', '29th-31st')
      
      #Getting January data
      janMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "1/")
      janWeekData = janMonthData$AccidentDatetime        #Preparing data to be sent into function
      janWeekOccurances = getAccidentsPerWeek(janWeekData, FALSE)   #Calls function, returns columns week1-week5snip
      janWeekData = data.frame(janWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting February data
      febMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "2/")
      febWeekData = febMonthData$AccidentDatetime        #Preparing data to be sent into function
      febWeekOccurances = getAccidentsPerWeek(febWeekData, FALSE)   #Calls function, returns columns week1-week5
      febWeekData = data.frame(febWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting March data
      marMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "3/")
      marWeekData = marMonthData$AccidentDatetime        #Preparing data to be sent into function
      marWeekOccurances = getAccidentsPerWeek(marWeekData, FALSE)   #Calls function, returns columns week1-week5
      marWeekData = data.frame(marWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting April data
      aprMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "4/")
      aprWeekData = aprMonthData$AccidentDatetime        #Preparing data to be sent into function
      aprWeekOccurances = getAccidentsPerWeek(aprWeekData, FALSE)   #Calls function, returns columns week1-week5
      aprWeekData = data.frame(aprWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting May data
      mayMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "5/")
      mayWeekData = mayMonthData$AccidentDatetime        #Preparing data to be sent into function
      mayWeekOccurances = getAccidentsPerWeek(mayWeekData, FALSE)   #Calls function, returns columns week1-week5
      mayWeekData = data.frame(mayWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting June data
      junMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "6/")
      junWeekData = junMonthData$AccidentDatetime        #Preparing data to be sent into function
      junWeekOccurances = getAccidentsPerWeek(junWeekData, FALSE)   #Calls function, returns columns week1-week5
      junWeekData = data.frame(junWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting July data
      julMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "7/")
      julWeekData = julMonthData$AccidentDatetime        #Preparing data to be sent into function
      julWeekOccurances = getAccidentsPerWeek(julWeekData, FALSE)   #Calls function, returns columns week1-week5
      julWeekData = data.frame(julWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting August data
      augMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "8/")
      augWeekData = augMonthData$AccidentDatetime        #Preparing data to be sent into function
      augWeekOccurances = getAccidentsPerWeek(augWeekData, FALSE)   #Calls function, returns columns week1-week5
      augWeekData = data.frame(augWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting September data
      sepMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "9/")
      sepWeekData = sepMonthData$AccidentDatetime        #Preparing data to be sent into function
      sepWeekOccurances = getAccidentsPerWeek(sepWeekData, FALSE)   #Calls function, returns columns week1-week5
      sepWeekData = data.frame(sepWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting October data
      octMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "10")
      octWeekData = octMonthData$AccidentDatetime        #Preparing data to be sent into function
      octWeekData
      octWeekOccurances = getAccidentsPerWeek(octWeekData, TRUE)   #Calls function, returns columns week1-week5
      octWeekOccurances
      octWeekData = data.frame(octWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting November data
      novMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "11")
      novWeekData = novMonthData$AccidentDatetime        #Preparing data to be sent into function
      novWeekOccurances = getAccidentsPerWeek(novWeekData, TRUE)   #Calls function, returns columns week1-week5
      novWeekData = data.frame(novWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting December data
      decMonthData = data2 %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "12")
      decWeekData = decMonthData$AccidentDatetime        #Preparing data to be sent into function
      decWeekOccurances = getAccidentsPerWeek(decWeekData, TRUE)   #Calls function, returns columns week1-week5
      decWeekData = data.frame(decWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      
      
      ######################
      # Step 3: Send the formatted data to become a graph
      #####################
      
      
      #Line graph showing the number of crashes over the year
      collisionYear_LG = outputLineGraph(monthData, monthData$months, monthData$monthNumbers, label = "Number of Crashes over the year", xlab = "Months", ylab = "Number of Crashes")
      
      #Line Graphs showing the number of crashes over the months
      janWeekGraph =outputLineGraph(janWeekData,reorder(janWeekData$weekRange, c(1,2,3,4,5)), 
                                    janWeekOccurances, label = "Number of Crashes over January", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      febWeekGraph =outputLineGraph(febWeekData,reorder(febWeekData$weekRange, c(1,2,3,4,5)), 
                                    febWeekOccurances, label = "Number of Crashes over February", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      marWeekGraph =outputLineGraph(marWeekData,reorder(marWeekData$weekRange, c(1,2,3,4,5)), 
                                    marWeekOccurances, label = "Number of Crashes over March", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      aprWeekGraph =outputLineGraph(aprWeekData,reorder(aprWeekData$weekRange, c(1,2,3,4,5)), 
                                    aprWeekOccurances, label = "Number of Crashes over April", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      mayWeekGraph =outputLineGraph(mayWeekData,reorder(mayWeekData$weekRange, c(1,2,3,4,5)), 
                                    mayWeekOccurances, label = "Number of Crashes over May", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      junWeekGraph =outputLineGraph(junWeekData,reorder(junWeekData$weekRange, c(1,2,3,4,5)), 
                                    junWeekOccurances, label = "Number of Crashes over June", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      julWeekGraph =outputLineGraph(julWeekData,reorder(julWeekData$weekRange, c(1,2,3,4,5)), 
                                    julWeekOccurances, label = "Number of Crashes over July", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      augWeekGraph =outputLineGraph(augWeekData,reorder(augWeekData$weekRange, c(1,2,3,4,5)), 
                                    augWeekOccurances, label = "Number of Crashes over August", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      sepWeekGraph =outputLineGraph(sepWeekData,reorder(sepWeekData$weekRange, c(1,2,3,4,5)), 
                                    sepWeekOccurances, label = "Number of Crashes over September", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      octWeekGraph =outputLineGraph(octWeekData,reorder(octWeekData$weekRange, c(1,2,3,4,5)), 
                                    octWeekOccurances, label = "Number of Crashes over October", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      novWeekGraph =outputLineGraph(novWeekData,reorder(novWeekData$weekRange, c(1,2,3,4,5)), 
                                    novWeekOccurances, label = "Number of Crashes over November", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      decWeekGraph =outputLineGraph(decWeekData,reorder(decWeekData$weekRange, c(1,2,3,4,5)), 
                                    decWeekOccurances, label = "Number of Crashes over December", 
                                    xlab = "Days of the week", ylab = "Number of Crashes")
      
      ######################
      # Step 4: Put the graphs on screen
      ######################
      
      COL4_render(output,janWeekGraph, febWeekGraph, marWeekGraph, aprWeekGraph, mayWeekGraph, junWeekGraph, julWeekGraph,
                  augWeekGraph, sepWeekGraph, octWeekGraph, novWeekGraph, decWeekGraph, collisionYear_LG)
    
    }
    else if(input$sidebar == "CON"){
      ######################
      # Step 1: read in the data
      ######################
      data <- read.csv("CONT.csv")
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(data, FALSE, TRUE, FALSE, FALSE, FALSE)
      CON_populateTopBar(session, numOfYears)
      
      if(input$CONSelect_Year == "Unselected" || input$CONSelect_Year == "All Years")
      {
        data <- data
      }
      else
      {
        data <- data %>% filter(str_sub(data$TicketDatetime, -17, -16) == str_sub(input$CONSelect_Year, -2, -1))
      }
      
      CON_populate_Widgets(session, data$Sex, data$Race, data$TicketType, data$Race)
      ######################
      # Step 2: Format the data
      ######################
      
      
      if(input$CON_Selector_1 != "Unselected"){
        data <- data %>% filter(Sex == input$CON_Selector_1)
      }
      if(input$CON_Selector_2 != "Unselected"){
        data <- data %>% filter(Race == input$CON_Selector_2)
      }
      if(input$CON_Selector_3 != "Unselected"){
        data <- data %>% filter(TicketType == input$CON_Selector_3)
      }
          
          
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      Contacts_Sex   <- outputPieChart (table(data$Sex), label = "Gender")
      Contacts_Race   <- outputBarPlot (table(data$Race), label = "Race")
      Contacts_TicketType <- outputBarPlot(table(data$TicketType), label = "Type")
      ######################
      # Step 4: Put the graphs on screen
      ######################
      CON_render(output, Contacts_Sex, Contacts_Race, Contacts_TicketType)
    }
    else if(input$sidebar == "OFF1"){
      ######################
      # Step 1: read in the data
      ######################
      data <- read.csv(file("CaseOffenses_2023.csv"))
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(data, FALSE, FALSE, TRUE, FALSE, FALSE)
      OFF1_populateTopBar(session, numOfYears)
      
      if(input$OFF1Year_Selector_By_CaseNumber == "Unselected" || input$OFF1Year_Selector_By_CaseNumber == "All Years")
      {
        data <- data
      }
      else
      {
        data <- data %>% filter(str_sub(CaseNumber, -11, -10) == str_sub(input$OFF1Year_Selector_By_CaseNumber, -2, -1))
      }
      
      OFF1_populate_Widgets(session, data$Counts, data$IBRCrimeCode)
      
      ######################
      # Step 2: Format the data
      ######################
  
      
      if(input$OFF1_Selector_1 != "Unselected"){
        data <- data %>% filter(Counts == input$OFF1_Selector_1)
      }
      if(input$OFF1_Selector_2 != "Unselected"){
        data <- data %>% filter(IBRCrimeCode == input$OFF1_Selector_2)
      }
      
      # For the Counts pie chart, find the most frequent Counts
      top_Counts <- data %>%
        group_by(Counts) %>%
        summarise(Frequency = n()) %>%
        arrange(desc(Frequency)) %>%
        slice_head(n = 6) %>%
        pull(Counts) # Extract the Counts values for filtering
      
      # Filter the original dataset to keep only the rows with the top 5 most frequent Counts
      data_filtered_for_top_Counts <- data %>%
        filter(Counts %in% top_Counts)
      
      # For the IBR Crime Code pie chart, find the top 5 most frequent IBR Crime Codes
      top_IBRCrimeCodes <- data %>%
        group_by(IBRCrimeCode) %>%
        summarise(Frequency = n()) %>%
        arrange(desc(Frequency)) %>%
        slice_head(n = 5) %>%
        pull(IBRCrimeCode) # Extract just the IBR Crime Code values for filtering
      
      # Filter the original dataset for the top 5 IBR Crime Codes
      data_filtered_for_top_IBRCrimeCode <- data %>%
        filter(IBRCrimeCode %in% top_IBRCrimeCodes)
      
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      # Generate the pie charts using the specifically prepared data
      Offenses_Counts <- outputPieChart(table(data_filtered_for_top_Counts$Counts), label = "Counts")
      
      Offenses_IBRCrimeCode <- outputPieChart(table(data_filtered_for_top_IBRCrimeCode$IBRCrimeCode), label = "IBR Crime Code")
      
      ######################
      # Step 4: Put the graphs on screen
      ######################
      OFF1_render(output, Offenses_Counts, Offenses_IBRCrimeCode)
    }
    
    
    # If block for call for service tab
    else if(input$sidebar == "OFF2") {
      ######################
      # Step 1: read in the data
      ######################
      data <- read.csv(file("Subjects_2023.csv"))
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(data, FALSE, FALSE, TRUE, TRUE, FALSE)
      numOfYears
      OFF2_populateTopBar(session, numOfYears)
      
      if(input$OFF2Year_Selector_By_CaseNumber == "Unselected" || input$OFF2Year_Selector_By_CaseNumber == "All Years")
      {
        data <- data
      }
      else
      {
        data <- data %>% filter(str_sub(CaseNumber, -11, -10) == str_sub(input$OFF2Year_Selector_By_CaseNumber, -2, -1))
      }
      #Populate widgets for OFF2
      OFF2_populate_Widgets(session, data$CaseSubjectSubType, data$CaseSubjectType)
      
      ######################
      # Step 2: Format the data
      ######################

      
      if(input$OFF2_Selector_1 != "Unselected"){
        data <- data$CaseSubjectSubType %>% filter(CaseSubjectSubType == input$OFF2_Selector_1)
      }
      if(input$OFF2_Selector_2 != "Unselected"){
        data <- data$CaseSubjectType %>% filter(CaseSubjectType == input$OFF2_Selector_2)
      }
      
      CaseSubjectSubType <- outputPieChart(table(data$CaseSubjectSubType), label = "Case Subject SubType")
      CaseSubjectType <- outputBarPlot(table(data$CaseSubjectType), label = "Case Subject Type")
      
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      ######################
      # Step 4: Put the graphs on screen
      ######################
      
      OFF2_render(output, CaseSubjectSubType, CaseSubjectType)
      
      
    }
    else if(input$sidebar == "OFF3"){
      ######################
      # Step 1: read in the data
      ######################
      data <- read.csv(file("Subjects_2023.csv"))
      
      numOfYears = findNumOfYears(data, FALSE, FALSE, TRUE, TRUE, FALSE)
      numOfYears
      OFF3_populateTopBar(session, numOfYears)
      
      if(input$OFF3Year_Selector_By_CaseNumber == "Unselected" || input$OFF3Year_Selector_By_CaseNumber == "All Years")
      {
        data <- data
      }
      else
      {
        data <- data %>% filter(str_sub(CaseNumber, -11, -10) == str_sub(input$OFF3Year_Selector_By_CaseNumber, -2, -1))
      }
      
      OFF3_populate_Widgets(session, data$Sex, data$Race, data$CaseSubjectType, data$CaseSubjectSubType)
      ######################
      # Step 2: Format the data
      ######################
    
      
      if(input$OFF3_Selector_1 != "Unselected"){
        data <- data %>% filter(Sex == input$OFF3_Selector_1)
      }
      if(input$OFF3_Selector_2 != "Unselected"){
        data <- data %>% filter(Race == input$OFF3_Selector_2)
      }
      if(input$OFF3_Selector_3 != "Unselected"){
        data <- data %>% filter(CaseSubjectType == input$OFF3_Selector_3)
      }
      if(input$OFF3_Selector_4 != "Unselected"){
        data <- data %>% filter(CaseSubjectSubType == input$OFF3_Selector_4)
      }
      
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      Offenses_Sex   <- outputPieChart (table(data$Sex), label = "Gender")
      Offenses_Race   <- outputBarPlot (table(data$Race), label = "Race")
      Offenses_SubjectType <- outputBarPlot (table(data$CaseSubjectType), label = "Subject Type")
      Offenses_SubjectSubType <- outputBarPlot (table(data$CaseSubjectSubType), label = "Sub-Type")
      ######################
      # Step 4: Put the graphs on screen
      ######################
      OFF3_render(output, Offenses_Sex, Offenses_Race, Offenses_SubjectType, Offenses_SubjectSubType)
    }
    ## this is for arrests
    else if(input$sidebar == "OFF4") {
      ######################
      # Step 1: read in the data
      ######################
      data <- read.csv(file("Arrests_2022.csv"))
      
      numOfYears = findNumOfYears(data, FALSE, FALSE, TRUE, FALSE, TRUE)
      numOfYears
      OFF4_populateTopBar(session, numOfYears)
      
      if(input$OFF4Year_Selector_By_CaseNumber == "Unselected" || input$OFF4Year_Selector_By_CaseNumber == "All Years")
      {
        data <- data
      }
      else
      {
        data <- data %>% filter(str_sub(CaseNumber, -11, -10) == str_sub(input$OFF4Year_Selector_By_CaseNumber, -2, -1))
      }
      
      # Populate widgets for OFF4 
      OFF4_populate_Widgets(session, data$Race, data$Sex, data$ArrestType, data$Description)
      ######################
      # Step 2: Format the data
      ######################
    
      
      if(input$OFF4_Selector_1 != "Unselected"){
        data <- data %>% filter(Race == input$OFF4_Selector_1)
      }
      if(input$OFF4_Selector_2 != "Unselected"){
        data <- data %>% filter(Sex == input$OFF4_Selector_2)
      }
      if(input$OFF4_Selector_3 != "Unselected"){
        data <- data %>% filter(ArrestType == input$OFF4_Selector_3)
      }
      if(input$OFF4_Selector_4 != "Unselected"){
        data <- data %>% filter(Description == input$OFF4_Selector_4)
      }
      
      
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      # Makes the graph for source of call
      Race_BP   <- outputBarPlot (table(data$Race        ), label = "Arrestee Race")
      # Makes the graph for police call status
      Sex_PC  <- outputPieChart(table(data$Sex  ), label = "Arrestee Gender")
      # Makes the graph for police call priority
      ArrType_BP  <- outputBarPlot (table(data$ArrestType), label = "Arrest Type")
      # Makes the graph for city
      # Calculate frequency of each description
      desc_freq <- data %>%
        group_by(Description) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>%
        top_n(8, Count) %>%
        ungroup() %>%
        mutate(Description = factor(Description, levels = Description))
      
      # Filter data to only include top 8 descriptions
      data_filtered <- data %>%
        filter(Description %in% desc_freq$Description)
      
      # Makes the graph for city with top 8 descriptions
      Desc_BP <- outputBarPlot(table(data_filtered$Description), label = "Description")
      # Desc_BP <- outputBarPlot(table(data$Description              ), label = "Description")
      
      ######################
      # Step 4: Put the graphs on screen
      ######################
      OFF4_render(output, Race_BP, Sex_PC, ArrType_BP, Desc_BP)
    }
  }
}

# This command runs our application- all you have to do to see the ouput is click
# the "Run App" button in the top right corner of RStudio.

# A webpage will open and allow you to interact with it. 
shinyApp(ui = ui, server = server)