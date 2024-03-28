# Makes the plot location with a selector widget
Plot_Maker <- function(tabName, plotName, widgetName){
  tab <- tabPanel(tabName, # Tab title
                  plotName,
    plotOutput(plotName), # plotOutput name
    # Graph controls
    selectInput(widgetName, widgetName, "Unselected", selected = 1))
  return(tab)
}


Plot_MakerWOSelect <- function(tabName, plotName){
  tab <- tabPanel(tabName, # Tab title
                  plotName,
                  plotOutput(plotName)) # plotOutput name

  return(tab)
}

# Updates the selector with its new selection
Selector_Updater <- function(session, selectorName, data, label = "Selector"){
  updateSelectInput(session, selectorName, 
                    label = label, 
                    choices = c("Unselected", unique(data)), 
                    selected = "Unselected")
}