library(shiny)          # The sever thingy
library(ggplot2)        # Used for plotting
library(dplyr)          # Used for data handling
library(shinydashboard) # Used for fancy UI stuff

# Import the tab files

source("tabs/CFStab.R")
source("tabs/COLtab.R")
source("tabs/UOFtab.R")
source("tabs/CItab.R")
source("tabs/CONtab.R")
source("tabs/OFFtab.R")
ui <- dashboardPage(
  ######################################################################
  ######################################################################
  #######                      Main page UI                     ########
  ####### UI people should be the only ones making changes here ########
  ######################################################################
  ######################################################################
  
  # Sets the title and also adds logo
  dashboardHeader(title='Norman PD', titleWidth = 275,
                  tags$li(class = "dropdown", imageOutput("logo", height = 50))),
  
  # Left sidebar, used to to get to major catogories
  dashboardSidebar(
    width = 275,
    sidebarMenu(
      # Variable name of this sidebar
      id = "sidebar",
      #             name on the sidebar for user                  var name in code      icon on screen
      menuItem("Calls for Service"                               , tabName = "CFS", icon = icon("phone")),
      menuItem("Collisions"                                      , tabName = "COL", icon = icon("car-burst"),
               menuSubItem('By Severity'                         , tabName = 'COLl', icon = icon('triangle-exclamation')),
               menuSubItem('By injury'                           , tabName = 'COL2', icon = icon('user-injured'))),
      
      menuItem("Use of Force"                                    , tabName = "UOF", icon = icon("gun")),
      #menuSubItem('Subjects by Resistance and Force'            , tabName = 'UOF4')),
      #menuSubItem('Subjects by Incidents and Demographics'      , tabName = 'UOF5')),
      menuItem("Complaints and Inquiries"                        , tabName = "CI", icon = icon("gun")),
      #menuSubItem('Incidents by Type and Disposition'           , tabName = 'CI1'),
      #menuSubItem('Subjects by Incidents and Demographics'      , tabName = 'CI2'),
      #menuSubItem('Subjects by Allegation and Finding'          , tabName = 'CI3'),
      
      menuItem("Contacts"                                        , tabName = "CON", icon = icon("hand")),
      #menuSubItem('Traffic and Parking Contacts'                , tabName = 'CON1')),
      menuItem("Offenses"                                        , tabName = "OFF", icon = icon("handcuffs")),
      #menuSubItem('Case Offenses'                               , tabName = 'OFF1',),
      #menuSubItem('Case Details'                                , tabName = 'OFF2',),
      #menuSubItem('Subjects'                                    , tabName = 'OFF3',),
      #menuSubItem('Arrests'                                     , tabName = 'OFF4',))
      menuItem("Tab1"                                            , tabName = "Tab1"),
      menuItem("Tab2"                                            , tabName = "Tab2"),
      menuItem("Tab3"                                            , tabName = "Tab3")
      
    )
  ),
  
  # Main body where graphs are rendered (they are all in their own files)
  dashboardBody(id = "tabs",
                tabItems(
                  CFS_tab(), # Calls for service tab
                  COL_tab(), # Collision Tab
                  UOF_tab(), # Use of force Tab
                  CI_tab(),  # Complaints and Inquiries Tab
                  CON_tab(), # Contacts Tab 
                  OFF_tab()  # Offense Tab
                ),
  )
)

# This is where we will define server logic. Ie, this is where we will parse the CSV,
# add graphs, create sliders/filters for user input, ect

# The bulk of our work will be here. Most of the time you will be 
# working in your teams trigger method

server <- function(input, output, session) {
  
  # Renders logo
  output$logo <- renderImage({
    list(src = "norman_pd_logo.jpeg", height = 100)
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
  outputBarPlot <- function(data, label = ""){
    plot <- renderPlot({# Put the plot at plotOutput("Barplot") in the shiny code
      graph <- ggplot(data.frame(data), aes(x = Var1, y = Freq, fill = Var1)) # Setup graph data
      graph = graph + geom_bar(stat = "identity", width = .8)                 # Set up the data as a bar chart
      graph = graph + xlab(label) + ylab("Amount")                                # Set the x/y labels
      graph = graph + guides(fill=guide_legend(title = label))                # Set the title of the legend
      graph = graph + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1))                   # Set the font size
      print(graph)                                                            # Print the graph
    }
    )
    return(plot)
  }
  
  # Makes a special barplot that is intended for data with long, descriptive labels. Legends are removed and a scroll should be added. 
  outputSpecialBarPlot <- function(data, label = ""){
    plot <- renderPlot({# Put the plot at plotOutput("Barplot") in the shiny code
      graph <- ggplot(data.frame(data), aes(x = Var1, y = Freq, fill = Var1)) # Setup graph data
      graph = graph + geom_bar(stat = "identity", width = .8, show.legend = FALSE)                 # Set up the data as a bar chart
      graph = graph + xlab(label) + ylab("Amount")                                # Set the x/y labels
      graph = graph + guides(fill=guide_legend(title = label))                # Set the title of the legend
      graph = graph + theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0))                  # Set the font size
      print(graph)                                                            # Print the graph
    }
    )
    return(plot)
  }
  
  # Makes a piechart object using the inputted data
  # This function is for step 3
  # data - the data that is to be rendered, must be tabled
  # label - string for graph labels
  outputPieChart <- function(data, label = ""){
    
    # Calculates percentage
    percent <- (data / sum(data)) * 100
    percent <- round(percent, digits = 2)
    
    # Renders the splot
    plot <- renderPlot({                                                     # Put the plot at plotOutput("Piechart") in the shiny code
      graph <- ggplot(data.frame(data), aes(x = "", y = Freq, fill = Var1))  # Set up graph data
      graph = graph + geom_bar(stat = "identity", width = 1)                 # Set up the data as a bar chart
      graph = graph + guides(fill=guide_legend(title = label))               # Set the title of the legend
      graph = graph + theme_void() + theme(text = element_text(size = 18))   # Remove the background and set the font size
      graph = graph + coord_polar("y", start = 0)                            # Convert the graph to polar
      graph = graph + geom_text(aes(label = percent), size = 6.5, position = position_dodge(width = 1)) # Adds the percentages to the pie chart
      print(graph)                                                           # Print the graph
    })
    return(plot)
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
      sex <- outputPieChart(table(data$SEX), label = "Sex")
      
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
      sex <- outputPieChart(table(data$SEX), label = "Sex")
      
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
    else if (input$sidebar == "CON")
    {
      # TODO (Daniel)
      
      ##########################
      # Step 1: Read in the data
      ##########################
      
      #########################
      # Step 2: Format the data
      #########################
      
      ###################################################
      # Step 3: Send the formatted data to become a graph
      ###################################################
      
      #################################################
      # Step 4: Render the graph, which will display it
      #################################################
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
      # Popultae the widgets in CFS
      CFS_populate_Widgets(session, input, data)
      ######################
      # Step 2: Filter the data
      ######################
      
      # If the user has selected an input for source of call then remove all that does not have the selected input
      if(input$CFS_Source_of_Call_Selector != "Unselected"){
        data <- data %>% filter(CallSource == input$CFS_Source_of_Call_Selector)
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
    else if(input$sidebar == "CON"){
      ######################
      # Step 1: read in the data
      ######################
      ######################
      # Step 2: Format the data
      ######################
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      ######################
      # Step 4: Put the graphs on screen
      ######################
    }
    else if(input$sidebar == "OFF"){
      ######################
      # Step 1: read in the data
      ######################
      ######################
      # Step 2: Format the data
      ######################
      ######################
      # Step 3: Send the formatted data to become a graph
      ######################
      ######################
      # Step 4: Put the graphs on screen
      ######################
    }
    # If block for call for service tab
    
  }
}

# This command runs our application- all you have to do to see the ouput is click
# the "Run App" button in the top right corner of RStudio.

# A webpage will open and allow you to interact with it. 
shinyApp(ui = ui, server = server)