library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Load data
data_uof <- read.csv("UOF.csv")
data_tpc <- read.csv("TPC.csv")

ui <- dashboardPage(
  dashboardHeader(title = 'Norman PD'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Use of Force", tabName = "UOF", icon = icon("dashboard")),
      menuItem("Calls for Service", tabName = "CFS", icon = icon("th")),
      menuItem("Contacts", tabName = "Contacts", icon = icon("users"))
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
            verbatimTextOutput("totalIncidentsUOF"),
            style = "font-size: 18px; color: #333; text-align: center;"
          )
        ),
        fluidRow(
          box(
            title = "Incident Type",
            plotOutput("incidentPlotUOF")
          ),
          box(
            title = "Involvement Type",
            plotOutput("involvementPlotUOF")
          )
        )
      ),
      
      # Calls for Service tab
      tabItem(
        tabName = "CFS",
        fluidRow(
          box(
            title = "Total Incidents",
            plotOutput("totalIncidentsCFS")
          )
        )
      ),
      
      # Contacts tab
      tabItem(
        tabName = "Contacts",
        fluidRow(
          box(
            title = "Race Distribution",
            plotOutput("raceDistribution")
          ),
          box(
            title = "Gender Distribution",
            plotOutput("genderDistribution")
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  # Total incidents count for Use of Force tab
  output$totalIncidentsUOF <- renderText({
    paste("Total Incidents (Use of Force):", nrow(data_uof))
  })
  
  # Total incidents count for Calls for Service tab
  output$totalIncidentsCFS <- renderText({
    paste("Total Incidents (Calls for Service):", nrow(data_tpc))
  })
  
  # Total incidents by incident type plot for Use of Force tab
  output$incidentPlotUOF <- renderPlot({
    ggplot(data_uof, aes(x = TicketType, fill = TicketType)) +
      geom_bar() +
      geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
      labs(title = "Total Incidents by Incident Type (Use of Force)", x = "Incident Type", y = "Total Incidents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Total incidents by involvement type plot for Use of Force tab
  output$involvementPlotUOF <- renderPlot({
    ggplot(data_uof, aes(x = INVOLVMENT, fill = INVOLVMENT)) +
      geom_bar() +
      geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
      labs(title = "Total Incidents by Involvement Type (Use of Force)", x = "Involvement Type", y = "Total Incidents") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Race distribution plot for Contacts tab
  output$raceDistribution <- renderPlot({
    ggplot(data_tpc, aes(x = Race, fill = Race)) +
      geom_bar() +
      geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
      labs(title = "Race Distribution", x = "Race", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Gender distribution plot for Contacts tab
  output$genderDistribution <- renderPlot({
    ggplot(data_tpc, aes(x = Sex, fill = Sex)) +
      geom_bar() +
      geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
      labs(title = "Gender Distribution", x = "Gender", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
