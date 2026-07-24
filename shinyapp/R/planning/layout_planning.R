

layout_planning <- function(id, title = "Planning"){
  
  # -- namespace
  ns <- NS(id)
  
  # -- return UI
  nav_panel(
    title = title,
    value = id,
    icon = icon("route"),
    class = "html-fill-item html-fill-container bslib-gap-spacing",
    style = "margin-top: 0px",
    
    # -- track title
    div(class = "track-title",
        h1(textOutput(ns("title"))),
        actionLink(inputId = paste0("close_", id), label = "close", onclick = 'Shiny.setInputValue(\"close_track\", this.id, {priority: \"event\"})')),

    leafletOutput(ns("map"))
    
  ) # nav_panel
  
}
