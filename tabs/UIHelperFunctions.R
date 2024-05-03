# Makes the plot location with a selector widget
#   tabName:    the display name of the tab
#   plotName:   the refence name of the table location
#   widgetName: the refence name of the widget, defaults to "" and if "" does not make a sector
Plot_Maker <- function(tabName, plotName, widgetName = ""){
  tab <- tabPanel(
    tabName,              # Tab title
    if(DEBUG)plotName,    # Will print the plots name on screen
    plotOutput(plotName), # Adds the plotOutput refence location
    
    # Graph controls, if widgetName is "" then widget is not added
    if(widgetName != "")selectInput(widgetName, widgetName, "Unselected", selected = 1)
  )
  return(tab)
}

# Plot Maker specifically for Allegations
Allegations_Plot_Maker <-function(tabName, plotName, widgetName = ""){
  tab <- tabPanel(
    tabName,        # Table title
    
    if(DEBUG)plotName,    # Will print the plot's name on screen
    (div(style = 'height:395px; overflow-y: scroll;', # Adds scroll
         plotOutput(plotName))),#Adds the plotOutput reference location
    
    
    if(widgetName !="")selectInput(widgetName, "Selector", c("Unselected", unique(data$ALLEGATION_MADE)), selected = 1)
    
  )
  return(tab)
}

# Updates the selector with its new selection
Selector_Updater <- function(session, selectorName, data, label = "Selector"){
  updateSelectInput(session, selectorName, 
                    label = label, 
                    choices = c("Unselected", unique(data)), 
                    selected = "Unselected")
}