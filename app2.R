library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

data <- read.csv("UOF.csv")

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
      # Use of Force tab
      tabItem(
        tabName = "UOF",
        fluidRow(
          box(
            title = "Total Incidents",
            verbatimTextOutput("totalIncidents"),
            style = "font-size: 18px; color: #333; text-align: center;"
          )
        ),
        fluidRow(
          box(
            title = "Incident Type",
            plotOutput("incidentPlot")
          ),
          box(
            title = "Involvement Type",
            plotOutput("involvementPlot")
          )
        ),
        fluidRow(
          box(
            title = "Subject Type",
            plotOutput("subjectPlot")
          ),
          box(
            title = "Subject Age Distribution",
            plotOutput("ageDistributionPlot")
          )
        ),
        fluidRow(
          box(
            title = "Incidents Distribution",
            selectInput("pie_toggle", "Toggle distribution by:", 
                        choices = c("Age", "Gender", "Incident Type"))
          ),
          box(
            title = "Incidents Distribution",
            plotOutput("pieChart")
          )
        )
      ),
      
      # Calls for Service tab
      tabItem(
        tabName = "CFS",
        fluidRow(
          box(
            title = "Total Incidents",
            plotOutput("totalIncidentsPlot")
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  # Total incidents count
  output$totalIncidents <- renderText({
    paste("Total Incidents:", nrow(data))
  })
  
  # Total incidents by incident type plot
  output$incidentPlot <- renderPlot({
    ggplot(data, aes(x = INCIDENT_TYPE, fill = INCIDENT_TYPE)) +
      geom_bar() +
      geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
      labs(title = "Total Incidents by Incident Type", x = "Incident Type", y = "Total Incidents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Total incidents by involvement type plot
  output$involvementPlot <- renderPlot({
    ggplot(data, aes(x = INVOLVMENT, fill = INVOLVMENT)) +
      geom_bar() +
      geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
      labs(title = "Total Incidents by Involvement Type", x = "Involvement Type", y = "Total Incidents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Total incidents by subject type plot
  output$subjectPlot <- renderPlot({
    ggplot(data, aes(x = SUBJ_TYPE, fill = SUBJ_TYPE)) +
      geom_bar() +
      geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
      labs(title = "Total Incidents by Subject Type", x = "Subject Type", y = "Total Incidents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Subject age distribution plot
  output$ageDistributionPlot <- renderPlot({
    ggplot(data, aes(x = AGE, fill = AGE)) +
      geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
      geom_histogram(binwidth = 5) +
      labs(title = "Subject Age Distribution", x = "Age", y = "Count") +
      theme_minimal()
  })
  
  # Pie chart based on toggle selection
  output$pieChart <- renderPlot({
    data_to_plot <- switch(input$pie_toggle,
                           "Age" = table(data$AGE),
                           "Gender" = table(data$SEX),
                           "Incident Type" = table(data$INCIDENT_TYPE))
    pie(data_to_plot, main = paste("Incidents Distribution by", input$pie_toggle),
        col = rainbow(length(data_to_plot)))
  })
  
  # Total incidents plot for Calls for Service tab
  output$totalIncidentsPlot <- renderPlot({
    ggplot(data, aes(x = INCIDENT_TYPE, fill = INCIDENT_TYPE)) +
      geom_bar() +
      geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
      labs(title = "Total Incidents", x = "Incident Type", y = "Total Incidents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}



shinyApp(ui = ui, server = server)
