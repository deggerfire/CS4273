#################################
# File for the call for service
#################################
# As of now (2/16) this is a bunch of demo stuff, so best not to read it
# unless you just want to
CFS_tab <- function(){
  tab <- tabItem(tabName = "CFS",
          tabBox(
            id = "CFS", height = "500px",
            tabPanel('Call Source Bargraph', plotOutput("CFS_table_1"), checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                                                                                           choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                                                                           selected = 1)),
            tabPanel("Call Source piechart", plotOutput("CFS_table_2")),
            tabPanel('Call Priority Bargraph', plotOutput("CFS_table_3")),
            tabPanel("Call Priority piechart", plotOutput("CFS_table_4"))
          ),
          tabBox(
            id = "CFS", height = "500px",
            tabPanel('Call Source Bargraph', plotOutput("CFS_table_5")),
            tabPanel("Call Source piechart", plotOutput("CFS_table_6"), checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                                                                                          choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                                                                          selected = 1)),
            tabPanel('Call Priority Bargraph', plotOutput("CFS_table_7")),
            tabPanel("Call Priority piechart", plotOutput("CFS_table_8"))
          ),
  )
  return(tab)
}

CFS_render <- function(output, plot1, plot2, plot3, plot4){
  output$CFS_table_1 <- plot1
  output$CFS_table_2 <- plot2
  output$CFS_table_3 <- plot3
  output$CFS_table_4 <- plot4
  
  output$CFS_table_5 <- plot1
  output$CFS_table_6 <- plot2
  output$CFS_table_7 <- plot3
  output$CFS_table_8 <- plot4
}