library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)

# Load data
data <- read.csv('UOF.csv')

ui <- dashboardPage(
  dashboardHeader(title = 'Norman PD'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Use of Force", tabName = "UOF", icon = icon("dashboard")),
      menuItem("Calls for Service", tabName = "CFS", icon = icon("th"))
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
      return(data %>% filter(SUBJ_TYPE == input$subjTypeFilter))
    }
  })
  
  # Filter for Incident Type
  incident_type_filter <- reactive({
    req(input$incidentTypeFilter)
    if (input$incidentTypeFilter == "All") {
      return(data)
    } else {
      return(data %>% filter(INCIDENT_TYPE == input$incidentTypeFilter))
    }
  })
  
  output$subPlot <- renderPlot({
    filtered_data <- subj_type_filter()
    subject_type_counts <- filtered_data %>% 
      group_by(SUBJ_TYPE) %>% 
      summarise(count = n())
    
    ggplot(subject_type_counts, aes(x = SUBJ_TYPE, y = count, fill = SUBJ_TYPE)) +
      geom_bar(stat = "identity") +
      xlab("Subject Type") +
      ylab("Count") +
      ggtitle("Distribution of Subject Types")
  })
  
  output$incidentPlot <- renderPlot({
    filtered_data <- incident_type_filter()
    incident_type_counts <- filtered_data %>% 
      group_by(INCIDENT_TYPE) %>% 
      summarise(count = n())
    
    ggplot(incident_type_counts, aes(x = "", y = count, fill = INCIDENT_TYPE)) +
      geom_bar(stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      ggtitle("Proportion of Incident Types")
  })
  
  output$CFS_Subject <- renderPlot({
    filtered_data <- subj_type_filter()
    subject_type_counts <- filtered_data %>% 
      group_by(SUBJ_TYPE) %>% 
      summarise(count = n())
    
    ggplot(subject_type_counts, aes(x = SUBJ_TYPE, y = count, fill = SUBJ_TYPE)) +
      geom_bar(stat = "identity") +
      xlab("Subject Type") +
      ylab("Count") +
      ggtitle("Distribution of Subject Types")
  })
  
  output$CFS_Incident <- renderPlot({
    filtered_data <- incident_type_filter()
    incident_type_counts <- filtered_data %>% 
      group_by(INCIDENT_TYPE) %>% 
      summarise(count = n())
    
    ggplot(incident_type_counts, aes(x = "", y = count, fill = INCIDENT_TYPE)) +
      geom_bar(stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      ggtitle("Proportion of Incident Types")
  })
  
  output$byAge <- renderPlot({
    ggplot(data, aes(x = AGE)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "Age Distribution",
           x = "Age",
           y = "Count")
  })
  
  output$byInvolvement <- renderPlot({
    data %>%
      group_by(INVOLVMENT) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = INVOLVMENT, y = count, fill = INVOLVMENT)) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = "Involvement Distribution",
           x = "Involvement Type",
           y = "Count") +
      coord_polar("y", start = 0) +
      theme_void()
  })
  
  # Subject Type Filter
  output$subjTypeFilter <- renderUI({
    selectInput("subjTypeFilter", "Filter by Subject Type:",
                c("All", unique(data$SUBJ_TYPE)))
  })
  
  # Incident Type Filter
  output$incidentTypeFilter <- renderUI({
    selectInput("incidentTypeFilter", "Filter by Incident Type:",
                c("All", unique(data$INCIDENT_TYPE)))
  })
  
}

shinyApp(ui = ui, server = server)




shinyApp(ui = ui, server = server)
