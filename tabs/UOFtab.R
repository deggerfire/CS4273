
UOF_tab <- function(){
  tab <- tabItem(tabName = "UOF",
                 fluidRow(
                    box(width = 6, height=500, plotOutput("subPlot", height = "500px")),
                    box( width = 6, height=500, plotOutput("incidentPlot", height = "500px"))
                  )
                )
  return(tab)
}