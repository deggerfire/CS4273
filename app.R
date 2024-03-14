library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)

# Load data
data <- read.csv('TPC.csv')

ui <- dashboardPage(
  dashboardHeader(title = 'Norman PD'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Use of Force", tabName = "UOF", icon = icon("dashboard")),
      menuItem("Calls for Service", tabName = "CFS", icon = icon("th")),
      menuItem("New Tab", tabName = "new_tab", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "UOF",
              fluidRow(
                box(width = 6, height = 500, plotOutput("subPlot", height = "500px")),
                box(width = 6, height = 500, plotOutput("incidentPlot", height = "500px"))
              )
      ),
      tabItem(tabName = "CFS",
              tabBox(
                title = "Tab Box 1",
                id = "SubjectTypePlot_CFS", height = "500px",
                tabPanel('Age Distribution', plotOutput("byAge")),
                tabPanel("Subject Type", plotOutput("CFS_Subject"))
              ),
              tabBox(
                title = "Tab Box 2",
                id = "IncidentTypePlot_CFS", height = "500px",
                tabPanel('By Involvement', plotOutput("byInvolvement")),
                tabPanel("Incident Type", plotOutput("CFS_Incident"))
              )
      ),
      tabItem(tabName = "new_tab",
              fluidRow(
                box(width = 6, height = 500, plotOutput("new_tab_plot1", height = "500px")),
                box(width = 6, height = 500, plotOutput("new_tab_plot2", height = "500px"))
              )
      )
    ),
    uiOutput("subjTypeFilter"),  # Add the Subject Type filter UI
    uiOutput("incidentTypeFilter")  # Add the Incident Type filter UI
  )
)


server <- function(input, output) {
  
  # Filter for Subject Type
  subj_type_filter <- reactive({
    req(input$subjTypeFilter)
    if (input$subjTypeFilter == "All") {
      return(data)
    } else {
      return(data %>% filter(Sex == input$subjTypeFilter)) # Assuming Sex as SUBJ_TYPE
    }
  })
  
  # Filter for Incident Type
  incident_type_filter <- reactive({
    req(input$incidentTypeFilter)
    if (input$incidentTypeFilter == "All") {
      return(data)
    } else {
      return(data %>% filter(TicketType == input$incidentTypeFilter)) # Assuming TicketType as INCIDENT_TYPE
    }
  })
  
  output$subPlot <- renderPlot({
    filtered_data <- subj_type_filter()
    subject_type_counts <- filtered_data %>% 
      group_by(Sex) %>% 
      summarise(count = n())
    
    ggplot(subject_type_counts, aes(x = Sex, y = count, fill = Sex)) +
      geom_bar(stat = "identity") +
      xlab("Subject Type") +
      ylab("Count") +
      ggtitle("Distribution of Subject Types")
  })
  
  output$incidentPlot <- renderPlot({
    filtered_data <- incident_type_filter()
    incident_type_counts <- filtered_data %>% 
      group_by(TicketType) %>% 
      summarise(count = n())
    
    ggplot(incident_type_counts, aes(x = "", y = count, fill = TicketType)) +
      geom_bar(stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      ggtitle("Proportion of Incident Types")
  })
  
  output$CFS_Subject <- renderPlot({
    filtered_data <- subj_type_filter()
    subject_type_counts <- filtered_data %>% 
      group_by(Sex) %>% 
      summarise(count = n())
    
    ggplot(subject_type_counts, aes(x = Sex, y = count, fill = Sex)) +
      geom_bar(stat = "identity") +
      xlab("Subject Type") +
      ylab("Count") +
      ggtitle("Distribution of Subject Types")
  })
  
  output$CFS_Incident <- renderPlot({
    filtered_data <- incident_type_filter()
    incident_type_counts <- filtered_data %>% 
      group_by(TicketType) %>% 
      summarise(count = n())
    
    ggplot(incident_type_counts, aes(x = "", y = count, fill = TicketType)) +
      geom_bar(stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      ggtitle("Proportion of Incident Types")
  })
  
  output$byAge <- renderPlot({
    ggplot(data, aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "Age Distribution",
           x = "Age",
           y = "Count")
  })
  
  output$byInvolvement <- renderPlot({
    data %>%
      group_by(Ethnicity) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = Ethnicity, y = count, fill = Ethnicity)) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = "Involvement Distribution",
           x = "Involvement Type",
           y = "Count") +
      coord_polar("y", start = 0) +
      theme_void()
  })
  
  # New Tab Plots
  output$new_tab_plot1 <- renderPlot({
    ggplot(data, aes(x = Race)) +
      geom_bar(fill = "blue") +
      labs(title = "Race Distribution",
           x = "Race",
           y = "Count")
  })
  
  output$new_tab_plot2 <- renderPlot({
    ggplot(data, aes(x = Age, y = TicketNumber)) +
      geom_point(color = "red") +
      labs(title = "Ticket Number vs Age",
           x = "Age",
           y = "Ticket Number")
  })
  
  # Subject Type Filter
  output$subjTypeFilter <- renderUI({
    selectInput("subjTypeFilter", "Filter by Subject Type:",
                c("All", unique(data$Sex)))
  })
  
  # Incident Type Filter
  output$incidentTypeFilter <- renderUI({
    selectInput("incidentTypeFilter", "Filter by Incident Type:",
                c("All", unique(data$TicketType)))
  })
  
}

shinyApp(ui = ui, server = server)
