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
#####################################################
#####################################################
#####################################################
####        Reading in all of the files          ####
#####################################################
#####################################################
#####################################################




#This data for CI and UOF have been merged, but not necessarily correctly.
#Also only has the yeasr 2023 and not 2016-2023 similar to the others.
CIdata <- read.csv("joined-data.csv")
UOFdata <- read.csv("joined-data.csv")

CFSdata <- read.csv(file("CFS_Merged_2023.csv"))
COL1data <- read.csv(file("COL_Merged_2023.csv"))
COL2data <- read.csv(file("COL_Injuries_Merged_2023.csv"))
COL3data <- read.csv(file("COL_Merged_2023.csv"))
COL4data <- read.csv(file("COL_Merged_2023.csv"))
CONdata <- read.csv("CON_Merged_2023.csv")

#Having to merge the CSV's since Excel would not do it. Also had to add NA to extra rows
#That were not consistent between the years.
OFF1data2022AndOlder <- read.csv(file("OFF_Merged_2022_AndOlder.csv"))
OFF1data2023AndNewer <- read.csv(file("OFF_2023_AndNewer.csv"))
OFF1data <- rbind(OFF1data2022AndOlder, OFF1data2023AndNewer)

OFF2data <- read.csv(file("OFF_SUB_Merge_2023.csv"))
OFF3data <- read.csv(file("OFF_SUB_Merge_2023.csv"))
OFF4data <- read.csv(file("OFF_Arrests_Merged_2023.csv"))


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
        if (nrow(data.frame(data)) == 0) {
          graph <- ggplot() +
            annotate("text", x = 0, y = 0, size = 10, label = "No Data to Graph") + theme_void()
        }
        else {
          graph <- ggplot(data.frame(data), aes(x = Var1, y = Freq, fill = Var1)) +
            geom_bar(stat = "identity", width = 0.8, show.legend = TRUE) +
            geom_text(aes(label = Freq), vjust = -0.5, size = 4) +  # Add numbers to bars
            labs(x = label, y = "Amount", fill = label) +
            theme_minimal() +
            theme(
              legend.title = element_blank(),
              axis.text.x = element_blank(), legend.position = "bottom",
              legend.text = element_text(size=8, hjust = 0.0),  # Align text to the left
              legend.box.margin = margin(0, 0, 0, 0),  # Box margin 
              legend.margin = margin(0, 0, 0, 0)  # Legend margin
            ) +
            guides(fill = guide_legend(nrow = 22, byrow = FALSE))
        }
        print(graph)
      }, error = function(e) {
        graph <- ggplot() + annotate("text", x = 0, y = 0, size = 10, label = "No Data to Graph") + theme_void()
        print(graph)
      })
    }, height = 700)
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
  findNumOfYears <- function(data, colName, left1, right1, left2, right2)
  {
    
    # data <- read.csv("CIDATA.csv")
    # UOFdata <- read.csv("UOFDATA.csv")
    # #colName is the name of the column that you want to search for
    # #"left" is the farthest left character in the string left1 for first line, 2 for last
    # #"right" is the farthest right character in the string right1 for first line, 2 for last
    # left1 = -6
    # left2 = -6
    # right1 = -5
    # right2= -5
    # colName = "FILENUM"
    numOfYear <- c()
    first = head(data, 1)
    last = tail(data, 1)
    first
    last
    first = first %>% select(contains(colName)) %>% str_sub(left1, right1)
    last = last %>%select(contains(colName)) %>% str_sub(left2, right2)
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
    return(numOfYear)
    
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
      
      ######################
      # Step 1: Gather UOF data
      ######################
      
      
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(UOFdata, "FILEN", -6, -5, -6, -5)
      
      UOF_populateTopBar(session, numOfYears)
      
      if(input$UOFSelect_Year == "Unselected" || input$UOFSelect_Year == "All Years")
      {
        UOFdata <- UOFdata 
        
      }
      else
      {
        UOFdata <- UOFdata %>% filter(str_sub(CreateDatetime.UTC., -6, -5) == str_sub(input$UOFSelect_Year, -2, -1))
        
      }
      # Populate the widgets in UOF
      
      # PLEASE PAY ATTENTION TO "YRS_EMPL", "INVOLVMENT" AND "SUBJ_TYPE" SPELLING
      UOF_populate_Widgets(session, UOFdata$RACE, UOFdata$SEX, UOFdata$YRS_EMPL, UOFdata$INVOLVMENT, UOFdata$AGE, UOFdata$SUBJ_TYPE) 
      
      ######################
      # Step 2: Filter the UOFdata
      ######################
      
      # If the user has selected an input for race then remove all that does not have the selected input
      if(input$UOF_Race_Selector != "Unselected"){
        UOFdata <- UOFdata %>% filter(RACE == input$UOF_Race_Selector)
      }
      
      
      ######################
      # Step 3: Send the formatted UOFdata to become a graph
      ######################
      
      
      # Replace CI values with NA so that CI UOFdata is filtered out
      UOFdata$RACE <- replace(UOFdata$RACE, grepl("Complaint", UOFdata$INCIDENT_TYPE), NA)
      
      # Fix UOFdata - Typos
      UOFdata$RACE <- sapply(UOFdata$RACE, function(x) {
        
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
        UOFdata <- UOFdata %>% filter(RACE == input$UOF_Race_Selector)
      }
      
      # Race Piechart
      R_PC <- outputPieChart(table(UOFdata$RACE), label = "Race")
      
      #--------------------
      
      # Replace CI values with NA so that CI UOFdata is filtered out
      UOFdata$SEX <- replace(UOFdata$SEX, grepl("Complaint", UOFdata$INCIDENT_TYPE), NA)
      
      # Fix UOFdata - Typos
      UOFdata$SEX <- sapply(UOFdata$SEX, function(x) {
        
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
        UOFdata <- UOFdata %>% filter(SEX == input$UOF_Sex_Selector)
      }
      
      # Sex Piechart
      S_PC <- outputPieChart(table(UOFdata$SEX), label = "Sex")
      
      #----------------------------------------------
      
      # Replace CI values with NA so that CI UOFdata is filtered out
      UOFdata$YRS_EMPL <- replace(UOFdata$YRS_EMPL, grepl("Complaint", UOFdata$INCIDENT_TYPE), NA)
      
      # Fix UOFdata - Too many values, sort them into ranges
      UOFdata$YRS_EMPL <- sapply(UOFdata$YRS_EMPL, function(x) 
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
        UOFdata <- UOFdata %>% filter(YRS_EMPL == input$UOF_Years_Employed_Selector)
      }
      
      # Years Employed Barplot
      YE_BP <- outputBarPlot(table(UOFdata$YRS_EMPL), label = "Years Employed")
      
      # ---------------------------
      
      
      # Replace CI values with NA so that CI UOFdata is filtered out
      UOFdata$INVOLVMENT <- replace(UOFdata$INVOLVMENT, grepl("Complaint", UOFdata$INCIDENT_TYPE), NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$UOF_Involvement_Selector != "Unselected"){
        UOFdata <- UOFdata %>% filter(INVOLVMENT == input$UOF_Involvement_Selector)
      }
      
      # Involvement Barplot
      I_BP <- outputBarPlot(table(UOFdata$INVOLVMENT), label = "Involvement")
      
      # ---------------
      
      # Replace UOF values with NA so that UOF UOFdata is filtered out
      UOFdata$AGE <- replace(UOFdata$AGE, grepl("Complaint", UOFdata$INCIDENT_TYPE), NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$UOF_Age_Selector != "Unselected"){
        UOFdata <- UOFdata %>% filter(AGE == input$UOF_Age_Selector)
      }
      
      # Age Barplot
      A_BP <- outputBarPlot(table(UOFdata$AGE), label = "Age")
      
      # -------------
      
      # Replace CI values with NA so that CI UOFdata is filtered out
      UOFdata$SUBJ_TYPE <- replace(UOFdata$SUBJ_TYPE, grepl("Complaint", UOFdata$INCIDENT_TYPE), NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$UOF_Subject_Type_Selector != "Unselected"){
        UOFdata <- UOFdata %>% filter(SUBJ_TYPE == input$UOF_Subject_Type_Selector)
      }
      
      # Subject_Type Barplot
      ST_BP <- outputBarPlot(table(UOFdata$SUBJ_TYPE), label = "Subject Type")
      
      
      ######################
      # Step 4: Put the graphs on screen
      ######################
      # Send the graphs off to the call for service render function to be put on scree
      
      
      UOF_render(output, R_PC, S_PC, YE_BP, I_BP, A_BP, ST_BP)
      
    }
    else if (input$sidebar == "CI") 
    {
      
      ######################
      # Step 1: Gather the CI data
      ######################
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(CIdata, "FILEN", -6, -5, -6, -5)

      CI_populateTopBar(session, numOfYears)

      
      if(input$CISelect_Year == "Unselected" || input$CISelect_Year == "All Years")
      {
        CIdata <- CIdata

      }
      else
      {
        CIdata <- CIdata %>% filter(str_sub(CreateDatetime.UTC., -6, -5) == str_sub(input$CISelect_Year, -2, -1))

      }
      
      # Populate the widgets in UOF
      CI_populate_Widgets(session, CIdata$RACE, CIdata$SEX, CIdata$YRS_EMPL, CIdata$ALLEGATION, CIdata$INVOLVMENT, CIdata$AGE, CIdata$SUBJ_TYPE) 
      
      ######################
      # Step 2: Filter the CIdata
      ######################
      
      
      # If the user has selected an input for race then remove all that does not have the selected input
      if(input$CI_Race_Selector != "Unselected"){
        CIdata <- CIdata %>% filter(RACE == input$CI_Race_Selector)
      }
      
      
      ######################
      # Step 3: Send the formatted CIdata to become a graph
      ######################
      
      
      # Replace UOF values with NA so that UOF CIdata is filtered out
      CIdata$RACE <- replace(CIdata$RACE, CIdata$INCIDENT_TYPE == "Use of force", NA)
      
      # Fix CIdata - Typos
      CIdata$RACE <- sapply(CIdata$RACE, function(x) {
        
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
        CIdata <- CIdata %>% filter(RACE == input$CI_Race_Selector)
      }
      
      # Race Piechart
      R_PC <- outputPieChart(table(CIdata$RACE), label = "Race")
      
      # ----------------------
      
      # Replace UOF values with NA so that UOF CIdata is filtered out
      CIdata$SEX <- replace(CIdata$SEX, CIdata$INCIDENT_TYPE == "Use of force", NA)
      
      # Fix CIdata - Typos
      CIdata$SEX <- sapply(CIdata$SEX, function(x) {
        
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
        CIdata <- CIdata %>% filter(SEX == input$CI_Sex_Selector)
      }
      
      # Sex Piechart
      S_PC <- outputPieChart(table(CIdata$SEX), label = "Sex")
      
      # ------------
      
      
      
      # Replace UOF values with NA so that UOF CIdata is filtered out
      CIdata$YRS_EMPL <- replace(CIdata$YRS_EMPL, CIdata$INCIDENT_TYPE == "Use of force", NA)
      
      # Fix CIdata - Too many values, sort them into ranges
      CIdata$YRS_EMPL <- sapply(CIdata$YRS_EMPL, function(x) 
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
        CIdata <- CIdata %>% filter(YRS_EMPL == input$CI_Years_Employed_Selector)
      }
      
      # Years Employed Barplot
      YE_BP <- outputBarPlot(table(CIdata$YRS_EMPL), label = "Years Employed")
      
      # ------------------------------- 
      
      # No Allegations in UOF, no need to filter
      
      # Fix CIdata -  Labels are too long, need to reduce number of characters
      CIdata$ALLEGATION_MADE <- sapply(CIdata$ALLEGATION_MADE, function(x) {
        if (is.na(x) || x == "Discrimination, Oppression or Favoritism - Color") # "Color" by itself is a strange allegation
        { 
          return(x)
        } else {
          return(strsplit(as.character(x), split = " - ")[[1]][2]) # Splits into before and after '-', using elements in [2]
        }
      })
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$CI_Allegations_Selector != "Unselected"){
        CIdata <- CIdata %>% filter(ALLEGATION_MADE == input$CI_Allegations_Selector)
      }
      
      # Allegations Special Barplot
      A_SBP <- outputSpecialBarPlot(table(CIdata$ALLEGATION_MADE), label = "Allegation")
      
      # --------------------------------
      
      
      # Replace UOF values with NA so that UOF CIdata is filtered out
      CIdata$INVOLVMENT <- replace(CIdata$INVOLVMENT, CIdata$INCIDENT_TYPE == "Use of force", NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$CI_Involvement_Selector != "Unselected"){
        CIdata <- CIdata %>% filter(INVOLVMENT == input$CI_Involvement_Selector)
      }
      
      # Involvement Barplot
      I_BP <- outputBarPlot(table(CIdata$INVOLVMENT), label = "Involvement")
      
      # -------------
      
      
      # Replace UOF values with NA so that UOF CIdata is filtered out
      CIdata$AGE <- replace(CIdata$AGE, CIdata$INCIDENT_TYPE == "Use of force", NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$CI_Age_Selector != "Unselected"){
        CIdata <- CIdata %>% filter(AGE == input$CI_Age_Selector)
      }
      
      # Age Barplot
      A_BP <- outputBarPlot(table(CIdata$AGE), label = "Age")
      
      
      # -------------------------
      
      # Replace UOF values with NA so that UOF CIdata is filtered out
      CIdata$SUBJ_TYPE <- replace(CIdata$SUBJ_TYPE, CIdata$INCIDENT_TYPE == "Use of force", NA)
      
      # Filters and updates graphs when user selects an option in this selector
      if(input$CI_Subject_Type_Selector != "Unselected"){
        CIdata <- CIdata %>% filter(SUBJ_TYPE == input$CI_Subject_Type_Selector)
      }
      
      # Subject Type Barplot
      ST_BP <- outputBarPlot(table(CIdata$SUBJ_TYPE), label = "Subject Type")
      
      ######################
      # Step 4: Put the graphs on screen
      ######################
      # Render
      
      CI_render(output, R_PC, S_PC, YE_BP, A_SBP, I_BP, A_BP, ST_BP)
      
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
      # Step 1: Gather the CFS CFSdata
      ######################
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(CFSdata, "Date",-8, -7, -8, -7)
      CFS_populateTopBar(session, numOfYears)
      
      if(input$CFSSelect_Year == "Unselected" || input$CFSSelect_Year == "All Years")
      {
        CFSdata <- CFSdata 
      }
      else
      {
        CFSdata <- CFSdata %>% filter(str_sub(CreateDatetime.UTC., -8, -7) == str_sub(input$CFSSelect_Year, -2, -1))
      }
      # Populate the widgets in CFS
      CFS_populate_Widgets(session, CFSdata$CallSource, CFSdata$PoliceCallStatus, CFSdata$PoliceCallPriority, CFSdata$City) #, CFSdata$PoliceCallType) This is the extra CFSdata for CFS tab
      ######################
      # Step 2: Filter the CFSdata
      ######################
      
      # If the user has selected an input for source of call then remove all that does not have the selected input
      
      if(input$CFS_Source_of_Call_Selector != "Unselected") {
        CFSdata <- CFSdata %>% filter(CallSource == input$CFS_Source_of_Call_Selector)
      }
      if(input$CFS_Police_Call_Status_Selector != "Unselected") {
        CFSdata <- CFSdata %>% filter(PoliceCallStatus == input$CFS_Police_Call_Status_Selector)
      }
      if(input$CFS_Police_Call_Priority_Selector != "Unselected") {
        CFSdata <- CFSdata %>% filter(PoliceCallPriority == input$CFS_Police_Call_Priority_Selector)
      }
      if(input$CFS_City_Selector != "Unselected") {
        CFSdata <- CFSdata %>% filter(City == input$CFS_City_Selector)
      }
      
      ######################
      # Step 3: Send the formatted CFSdata to become a graph
      ######################
      # Makes the graph for source of call
      CS_BP   <- outputBarPlot (table(CFSdata$CallSource        ), label = "Source of Call")
      # Makes the graph for police call status
      PCS_PC  <- outputPieChart(table(CFSdata$PoliceCallStatus  ), label = "PoliceCallStatus")
      # Makes the graph for police call priority
      PCP_BP  <- outputBarPlot (table(CFSdata$PoliceCallPriority), label = "PoliceCallPriority")
      # Makes the graph for city
      City_PC <- outputPieChart(table(CFSdata$City              ), label = "City")
      
      ######################
      # Step 4: Put the graphs on screen
      ######################
      # Send the graphs off to the call for service render function to be put on screen
      CFS_render(output, CS_BP, PCS_PC, PCP_BP, City_PC)
      
    }
    else if(input$sidebar == "COL1") {
      ######################
      # Step 1: Gather the COL1 data
      ######################
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(COL1data, "Date", -7, -6, -7, -6)

      COL1_populateTopBar(session, numOfYears)
      
      if(input$COL1Select_Year == "Unselected" || input$COL1Select_Year == "All Years")
      {
        COL1data <- COL1data
      }
      else
      {
        COL1data <- COL1data %>% filter(str_sub(AccidentDatetime, -2, -1) == str_sub(input$COL1Select_Year, -2, -1))
      }
      
      # Populate widgets for COL1 
      COL1_populate_Widgets(session, COL1data$UnitType, COL1data$DriverPedCondition, COL1data$ChemicalTest, COL1data$ContributingFactors)#COL1data$UnitType) #Useless COL1data? Not being used currently ################################
      ######################
      # Step 2: Format the COL1data
      ######################
      
      if(input$COL1_Selector_1 != "Unselected"){
        COL1data <- COL1data %>% filter(UnitType == input$COL1_Selector_1)
      }
      if(input$COL1_Selector_2 != "Unselected"){
        COL1data <- COL1data %>% filter(DriverPedCondition == input$COL1_Selector_2)
      }
      if(input$COL1_Selector_3 != "Unselected"){
        COL1data <- COL1data %>% filter(ChemicalTest == input$COL1_Selector_3)
      }
      if(input$COL1_Selector_4 != "Unselected"){
        COL1data <- COL1data %>% filter(ContributingFactors == input$COL1_Selector_4)
      }
      ######################
      # Step 3: Send the formatted COL1data to become a graph
      ######################
      # Makes the graph for source of call
      UnType_PC   <- outputPieChart (table(COL1data$UnitType        ), label = "Unit Type")
      # Makes the graph for police call status
      ChemTest_PC  <- outputPieChart(table(COL1data$ChemicalTest  ), label = "Chemical Test")
      # Makes the graph for police call priority
      DPC_BP  <- outputBarPlot (table(COL1data$DriverPedCondition), label = "Driver Condition")
      # Makes the graph for city
      
      CF_freq <- COL1data %>%
        group_by(ContributingFactors) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>%
        top_n(8, Count) %>%
        ungroup() %>%
        mutate(ContributingFactors = factor(ContributingFactors, levels = ContributingFactors))
      
      # Filter COL1data to only include top 8 descriptions
      COL1data_filtered <- COL1data %>%
        filter(ContributingFactors %in% CF_freq$ContributingFactors)
      
      # Makes the graph for city with top 8 descriptions
      CF_BP <- outputBarPlot(table(COL1data_filtered$ContributingFactors), label = "Contributing Factors")
      
      ######################
      # Step 4: Put the graphs on screen
      ######################
      COL1_render(output, UnType_PC, DPC_BP, ChemTest_PC, CF_BP)
    }
    else if(input$sidebar == "COL2")
    {
      ######################
      # Step 1: Gather the COL2 data
      ######################

      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(COL2data, "Date", -7, -6, -7, -6)
      COL2_populateTopBar(session, numOfYears)
      
      if(input$COL2Select_Year == "Unselected" || input$COL2Select_Year == "All Years")
      {
        COL2data <- COL2data
      }
      else
      {
        COL2data <- COL2data %>% filter(str_sub(AccidentDatetime, -7, -6) == str_sub(input$COL2Select_Year, -2, -1))
      }
      #Populate widgets for COL2
      COL2_populate_Widgets(session, COL2data$Description, COL2data$Sev_Num)
      
      ######################
      # Step 2: Format the data
      ######################
      
      desc <- outputPieChart(table(COL2data$Description), label = "Outcome")
      severity <- outputBarPlot(table(COL2data$Sev_Num), label = "Severity Number")
      
      
      if(input$COL2_Selector_1 != "Unselected"){
        COL2data <- COL2data %>% filter(Description == input$COL2_Selector_1)
      }
      if(input$COL2_Selector_2 != "Unselected"){
        COL2data <- COL2data %>% filter(Sev_Num == input$COL2_Selector_2)
      }
      
      
      ######################
      # Step 3: Send the formatted data to become a graph
      #####################
      COL2_render(output, desc, severity)
    }
    else if(input$sidebar == "COL3")
    {
      ######################
      # Step 1: Gather the COL3 data
      ######################
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(COL3data, "Date", -7, -6, -7, -6)
      COL3_populateTopBar(session, numOfYears)
      
      if(input$COL3Select_Year == "Unselected" || input$COL3Select_Year == "All Years")
      {
        COL3data <- COL3data
      }
      else
      {
        COL3data <- COL3data %>% filter(str_sub(AccidentDatetime, -2, -1) == str_sub(input$COL3Select_Year, -2, -1))
      }
      ######################
      # Step 2: Format the data
      ######################
      
      
      #Thirteen streets per dataSet, Each graph is by first letter in street. 
      streetAB = COL3data %>% select(StreetType, StreetName) %>% filter(StreetType == "ST") %>% filter(substr(StreetName, 1, 1) == "A" 
                                                                                                    | substr(StreetName, 1, 1) == "B")
      
      streetCG = COL3data %>% select(StreetType, StreetName) %>% filter(StreetType == "ST") %>% filter(substr(StreetName, 1, 1) == "C" 
                                                                                                    | substr(StreetName, 1, 1) == "D" 
                                                                                                    | substr(StreetName, 1, 1) == "E" 
                                                                                                    | substr(StreetName, 1, 1) == "F" 
                                                                                                    | substr(StreetName, 1, 1) == "G")
      
      streetHL = COL3data %>% select(StreetType, StreetName) %>% filter(StreetType == "ST") %>% filter(substr(StreetName, 1, 1) == "H"
                                                                                                    | substr(StreetName, 1, 1) == "I"
                                                                                                    | substr(StreetName, 1, 1) == "J"
                                                                                                    | substr(StreetName, 1, 1) == "J"
                                                                                                    | substr(StreetName, 1, 1) == "K"
                                                                                                    | substr(StreetName, 1, 1) == "L")
      
      
      streetMZ = COL3data %>% select(StreetType, StreetName) %>% filter(StreetType == "ST") %>% filter(substr(StreetName, 1, 1) == "M" 
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
      mainStreets = COL3data %>% select(StreetType, StreetName) %>% filter(StreetType == "ST") %>% filter(StreetName == "LINDSEY"
                                                                                                       | StreetName == "BOYD"
                                                                                                       | StreetName == "ALAMEDA"
                                                                                                       | StreetName == "GRAY"
                                                                                                       | StreetName == "MAIN"
                                                                                                       | StreetName == "ROBINSON")
      
      
      
      
      
      ######################
      # Step 3: Send the formatted data to become a graph
      #####################
      
      # Graphs for comparing number of accidents on streets through a bar chart
      SN_BP1 = outputBarPlot(table(streetAB$StreetName ), label = "Streets A-B")
      SN_BP2 = outputBarPlot(table(streetCG$StreetName ), label = "Streets C-G")
      SN_BP3 = outputBarPlot(table(streetHL$StreetName ), label = "Streets H-L")
      SN_BP4 = outputBarPlot(table(streetMZ$StreetName ), label = "Streets M-Z")
      
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
      # Step 1: Gather the COL4 data
      ######################
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(COL4data, "Date", -7, -6, -7, -6)
      COL4_populateTopBar(session, numOfYears)
      
      if(input$COL4Select_Year == "Unselected" || input$COL4Select_Year == "All Years")
      {
        COL4data <- COL4data
      }
      else
      {
        COL4data <- COL4data %>% filter(str_sub(AccidentDatetime, -2, -1) == str_sub(input$COL4Select_Year, -2, -1))
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
      for (i in 1:length(substr(COL4data$AccidentDatetime, 1, 2))){
        x = substr(COL4data$AccidentDatetime[i], 1, 2)
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
      janMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "1/")
      janWeekData = janMonthData$AccidentDatetime        #Preparing data to be sent into function
      janWeekOccurances = getAccidentsPerWeek(janWeekData, FALSE)   #Calls function, returns columns week1-week5snip
      janWeekData = data.frame(janWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting February data
      febMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "2/")
      febWeekData = febMonthData$AccidentDatetime        #Preparing data to be sent into function
      febWeekOccurances = getAccidentsPerWeek(febWeekData, FALSE)   #Calls function, returns columns week1-week5
      febWeekData = data.frame(febWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting March data
      marMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "3/")
      marWeekData = marMonthData$AccidentDatetime        #Preparing data to be sent into function
      marWeekOccurances = getAccidentsPerWeek(marWeekData, FALSE)   #Calls function, returns columns week1-week5
      marWeekData = data.frame(marWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting April data
      aprMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "4/")
      aprWeekData = aprMonthData$AccidentDatetime        #Preparing data to be sent into function
      aprWeekOccurances = getAccidentsPerWeek(aprWeekData, FALSE)   #Calls function, returns columns week1-week5
      aprWeekData = data.frame(aprWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting May data
      mayMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "5/")
      mayWeekData = mayMonthData$AccidentDatetime        #Preparing data to be sent into function
      mayWeekOccurances = getAccidentsPerWeek(mayWeekData, FALSE)   #Calls function, returns columns week1-week5
      mayWeekData = data.frame(mayWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting June data
      junMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "6/")
      junWeekData = junMonthData$AccidentDatetime        #Preparing data to be sent into function
      junWeekOccurances = getAccidentsPerWeek(junWeekData, FALSE)   #Calls function, returns columns week1-week5
      junWeekData = data.frame(junWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting July data
      julMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "7/")
      julWeekData = julMonthData$AccidentDatetime        #Preparing data to be sent into function
      julWeekOccurances = getAccidentsPerWeek(julWeekData, FALSE)   #Calls function, returns columns week1-week5
      julWeekData = data.frame(julWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting August data
      augMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "8/")
      augWeekData = augMonthData$AccidentDatetime        #Preparing data to be sent into function
      augWeekOccurances = getAccidentsPerWeek(augWeekData, FALSE)   #Calls function, returns columns week1-week5
      augWeekData = data.frame(augWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting September data
      sepMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "9/")
      sepWeekData = sepMonthData$AccidentDatetime        #Preparing data to be sent into function
      sepWeekOccurances = getAccidentsPerWeek(sepWeekData, FALSE)   #Calls function, returns columns week1-week5
      sepWeekData = data.frame(sepWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting October data
      octMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "10")
      octWeekData = octMonthData$AccidentDatetime        #Preparing data to be sent into function
      octWeekData
      octWeekOccurances = getAccidentsPerWeek(octWeekData, TRUE)   #Calls function, returns columns week1-week5
      octWeekOccurances
      octWeekData = data.frame(octWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting November data
      novMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "11")
      novWeekData = novMonthData$AccidentDatetime        #Preparing data to be sent into function
      novWeekOccurances = getAccidentsPerWeek(novWeekData, TRUE)   #Calls function, returns columns week1-week5
      novWeekData = data.frame(novWeekOccurances, weekRange) #Creating the dataframe to be graphed
      
      #Getting December data
      decMonthData = COL4data %>% select(AccidentDatetime) %>% filter(substr(AccidentDatetime, 1, 2) == "12")
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
      # Step 1: read in the CON data
      ######################
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(CONdata, "Date", -11, -10, -11, -10)
      numOfYears
      CON_populateTopBar(session, numOfYears)
      
      if(input$CONSelect_Year == "Unselected" || input$CONSelect_Year == "All Years")
      {
        CONdata <- CONdata
      }
      else
      {
        CONdata <- CONdata %>% filter(str_sub(CONdata$TicketDatetime, -11, -10) == str_sub(input$CONSelect_Year, -2, -1))
      }
      
      
      CON_populate_Widgets(session, CONdata$Race, CONdata$Sex, CONdata$TicketType)
      ######################
      # Step 2: Format the CON data
      ######################
      
      
      if(input$CON_Race_Selector != "Unselected"){
        CONdata <- CONdata %>% filter(Race == input$CON_Race_Selector)
      }
      if(input$CON_Sex_Selector != "Unselected"){
        CONdata <- CONdata %>% filter(Sex == input$CON_Sex_Selector)
      }
      if(input$CON_Ticket_Type_Selector != "Unselected"){
        CONdata <- CONdata %>% filter(TicketType == input$CON_Ticket_Type_Selector)
      }
      
      
      ######################
      # Step 3: Send the formatted CON data to become a graph
      ######################
      Contacts_Race   <- outputPieChart (table(CONdata$Race), label = "Race")
      Contacts_Sex   <- outputPieChart (table(CONdata$Sex), label = "Sex")
      
      Contacts_TicketType <- outputBarPlot(table(CONdata$TicketType), label = "Ticket Type")
      ######################
      # Step 4: Put the graphs on screen
      ######################
      CON_render(output, Contacts_Race, Contacts_Sex, Contacts_TicketType)
    }
    else if(input$sidebar == "OFF1"){
      ######################
      # Step 1: Gather the OFF1 data
      ######################
      
      numOfYears = findNumOfYears(OFF1data, "CaseN", -11, -10, -11, -10)
      
      
      OFF1_populateTopBar(session, numOfYears)
      
    
      if(input$OFF1Year_Selector_By_CaseNumber == "Unselected" || input$OFF1Year_Selector_By_CaseNumber == "All Years")
      {
        OFF1data <- OFF1data
      }
      else
      {
        OFF1data <- OFF1data %>% filter(str_sub(CaseNumber, -11, -10) == str_sub(input$OFF1Year_Selector_By_CaseNumber, -2, -1))
      }
      
      
      OFF1_populate_Widgets(session, OFF1data$Counts, OFF1data$IBRCrimeCode)
      
      ######################
      # Step 2: Format the OFF1 data
      ######################
      
      
      if(input$OFF1_Selector_1 != "Unselected"){
        OFF1data <- OFF1data %>% filter(Counts == input$OFF1_Selector_1)
      }
      if(input$OFF1_Selector_2 != "Unselected"){
        OFF1data <- OFF1data %>% filter(IBRCrimeCode == input$OFF1_Selector_2)
      }
      
      # For the Counts pie chart, find the most frequent Counts
      top_Counts <- OFF1data %>%
        group_by(Counts) %>%
        summarise(Frequency = n()) %>%
        arrange(desc(Frequency)) %>%
        slice_head(n = 6) %>%
        pull(Counts) # Extract the Counts values for filtering
      
      # Filter the original OFF1 dataset to keep only the rows with the top 5 most frequent Counts
      OFF1data_filtered_for_top_Counts <- OFF1data %>%
        filter(Counts %in% top_Counts)
      
      # For the IBR Crime Code pie chart, find the top 5 most frequent IBR Crime Codes
      top_IBRCrimeCodes <- OFF1data %>%
        group_by(IBRCrimeCode) %>%
        summarise(Frequency = n()) %>%
        arrange(desc(Frequency)) %>%
        slice_head(n = 5) %>%
        pull(IBRCrimeCode) # Extract just the IBR Crime Code values for filtering
      
      # Filter the original OFF1 dataset for the top 5 IBR Crime Codes
      OFF1data_filtered_for_top_IBRCrimeCode <- OFF1data %>%
        filter(IBRCrimeCode %in% top_IBRCrimeCodes)
      
      ######################
      # Step 3: Send the formatted OFF1 data to become a graph
      ######################
      # Generate the pie charts using the specifically prepared OFF1data
      Offenses_Counts <- outputPieChart(table(OFF1data_filtered_for_top_Counts$Counts), label = "Counts")
      
      Offenses_IBRCrimeCode <- outputPieChart(table(OFF1data_filtered_for_top_IBRCrimeCode$IBRCrimeCode), label = "IBR Crime Code")
      
      ######################
      # Step 4: Put the graphs on screen
      ######################
      OFF1_render(output, Offenses_Counts, Offenses_IBRCrimeCode)
    }
    
    
    # If block for call for service tab
    else if(input$sidebar == "OFF2") {
      ######################
      # Step 1: Gather the OFF2 data
      ######################
      
      #Getting the number of years and then populating the top widget
      numOfYears = findNumOfYears(OFF2data, "CaseN", -11, -10, -11, -10)
      numOfYears
      OFF2_populateTopBar(session, numOfYears)
      
      if(input$OFF2Year_Selector_By_CaseNumber == "Unselected" || input$OFF2Year_Selector_By_CaseNumber == "All Years")
      {
        OFF2data <- OFF2data
      }
      else
      {
        OFF2data <- OFF2data %>% filter(str_sub(CaseNumber, -11, -10) == str_sub(input$OFF2Year_Selector_By_CaseNumber, -2, -1))
      }
      #Populate widgets for OFF2
      OFF2_populate_Widgets(session, OFF2data$CaseSubjectSubType, OFF2data$CaseSubjectType)
      
      ######################
      # Step 2: Format the OFF2 data
      ######################
      
      
      if(input$OFF2_Selector_1 != "Unselected"){
        OFF2data <- OFF2data$CaseSubjectSubType %>% filter(CaseSubjectSubType == input$OFF2_Selector_1)
      }
      if(input$OFF2_Selector_2 != "Unselected"){
        OFF2data <- OFF2data$CaseSubjectType %>% filter(CaseSubjectType == input$OFF2_Selector_2)
      }
      
      CaseSubjectSubType <- outputPieChart(table(OFF2data$CaseSubjectSubType), label = "Case Subject SubType")
      CaseSubjectType <- outputBarPlot(table(OFF2data$CaseSubjectType), label = "Case Subject Type")
      
      ######################
      # Step 3: Send the formatted OFF2data to become a graph
      ######################
      ######################
      # Step 4: Put the graphs on screen
      ######################
      
      OFF2_render(output, CaseSubjectSubType, CaseSubjectType)
      
      
    }
    else if(input$sidebar == "OFF3"){
      ######################
      # Step 1: read in the OFF3 data
      ######################
      
      numOfYears = findNumOfYears(OFF3data, "CaseN", -11, -10, -11, -10)
      numOfYears
      OFF3_populateTopBar(session, numOfYears)
      
      if(input$OFF3Year_Selector_By_CaseNumber == "Unselected" || input$OFF3Year_Selector_By_CaseNumber == "All Years")
      {
        OFF3data <- OFF3data
      }
      else
      {
        OFF3data <- OFF3data %>% filter(str_sub(CaseNumber, -11, -10) == str_sub(input$OFF3Year_Selector_By_CaseNumber, -2, -1))
      }
      
      OFF3_populate_Widgets(session, OFF3data$Sex, OFF3data$Race, OFF3data$CaseSubjectType, OFF3data$CaseSubjectSubType)
      ######################
      # Step 2: Format the OFF3 data
      ######################
      
      
      if(input$OFF3_Selector_1 != "Unselected"){
        OFF3data <- OFF3data %>% filter(Sex == input$OFF3_Selector_1)
      }
      if(input$OFF3_Selector_2 != "Unselected"){
        OFF3data <- OFF3data %>% filter(Race == input$OFF3_Selector_2)
      }
      if(input$OFF3_Selector_3 != "Unselected"){
        OFF3data <- OFF3data %>% filter(CaseSubjectType == input$OFF3_Selector_3)
      }
      if(input$OFF3_Selector_4 != "Unselected"){
        OFF3data <- OFF3data %>% filter(CaseSubjectSubType == input$OFF3_Selector_4)
      }
      
      ######################
      # Step 3: Send the formatted OFF3data to become a graph
      ######################
      Offenses_Sex   <- outputPieChart (table(OFF3data$Sex), label = "Gender")
      Offenses_Race   <- outputBarPlot (table(OFF3data$Race), label = "Race")
      Offenses_SubjectType <- outputBarPlot (table(OFF3data$CaseSubjectType), label = "Subject Type")
      Offenses_SubjectSubType <- outputBarPlot (table(OFF3data$CaseSubjectSubType), label = "Sub-Type")
      ######################
      # Step 4: Put the graphs on screen
      ######################
      OFF3_render(output, Offenses_Sex, Offenses_Race, Offenses_SubjectType, Offenses_SubjectSubType)
    }
    ## this is for arrests
    else if(input$sidebar == "OFF4") {
      ######################
      # Step 1: Gather the OFF4 data
      ######################
      
      numOfYears = findNumOfYears(OFF4data, "CaseN",-11, -10, -11, -10)
      numOfYears
      OFF4_populateTopBar(session, numOfYears)
      
      if(input$OFF4Year_Selector_By_CaseNumber == "Unselected" || input$OFF4Year_Selector_By_CaseNumber == "All Years")
      {
        OFF4data <- OFF4data
      }
      else
      {
        OFF4data <- OFF4data %>% filter(str_sub(CaseNumber, -11, -10) == str_sub(input$OFF4Year_Selector_By_CaseNumber, -2, -1))
      }
      
      # Populate widgets for OFF4 
      OFF4_populate_Widgets(session, OFF4data$Race, OFF4data$Sex, OFF4data$ArrestType, OFF4data$Description)
      ######################
      # Step 2: Format the OFF4 data
      ######################
      
      
      if(input$OFF4_Selector_1 != "Unselected"){
        OFF4data <- OFF4data %>% filter(Race == input$OFF4_Selector_1)
      }
      if(input$OFF4_Selector_2 != "Unselected"){
        OFF4data <- OFF4data %>% filter(Sex == input$OFF4_Selector_2)
      }
      if(input$OFF4_Selector_3 != "Unselected"){
        OFF4data <- OFF4data %>% filter(ArrestType == input$OFF4_Selector_3)
      }
      if(input$OFF4_Selector_4 != "Unselected"){
        OFF4data <- OFF4data %>% filter(Description == input$OFF4_Selector_4)
      }
      
      
      ######################
      # Step 3: Send the formatted OFF4 data to become a graph
      ######################
      # Makes the graph for source of call
      Race_BP   <- outputBarPlot (table(OFF4data$Race        ), label = "Arrestee Race")
      # Makes the graph for police call status
      Sex_PC  <- outputPieChart(table(OFF4data$Sex  ), label = "Arrestee Gender")
      # Makes the graph for police call priority
      ArrType_BP  <- outputBarPlot (table(OFF4data$ArrestType), label = "Arrest Type")
      # Makes the graph for city
      # Calculate frequency of each description
      desc_freq <- OFF4data %>%
        group_by(Description) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>%
        top_n(8, Count) %>%
        ungroup() %>%
        mutate(Description = factor(Description, levels = Description))
      
      # Filter OFF4data to only include top 8 descriptions
      OFF4data_filtered <- OFF4data %>%
        filter(Description %in% desc_freq$Description)
      
      # Makes the graph for city with top 8 descriptions
      Desc_BP <- outputBarPlot(table(OFF4data_filtered$Description), label = "Description")
      # Desc_BP <- outputBarPlot(table(OFF4data$Description              ), label = "Description")
      
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