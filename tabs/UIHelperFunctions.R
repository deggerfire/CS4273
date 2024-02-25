# Makes the plot location with a selector widget
Plot_Maker <- function(tabName, plotName, widgetName){
  tab <- tabPanel(tabName, # Tab title
    plotOutput(plotName), # plotOutput name
    # Graph controls
    selectInput(widgetName, "Selector", "Unselected", selected = 1))
  return(tab)
}

# Updates the selector with its new selection
Selector_Updater <- function(session, selectorName, data){
  updateSelectInput(session, selectorName, 
                    label = "Selector", 
                    choices = c("Unselected", unique(data)), 
                    selected = "Unselected")
}