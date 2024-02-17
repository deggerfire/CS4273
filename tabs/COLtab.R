COL_tab <- function(){
  tab <- tabItem(tabName = "COL",
                 tabBox(
                   id = "SubjectTypePlot_CFS", height = "500px",
                   tabPanel('REEEEEE', plotOutput("thing"))
                 )
  )
  return(tab)
}