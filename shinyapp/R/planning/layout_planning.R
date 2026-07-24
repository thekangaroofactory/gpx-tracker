

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

    # -- track info
    layout_column_wrap(
      fill = F,
      p(textOutput(ns("nb_day"), inline = T), "itinerary"),
      p("Distance:", textOutput(ns("distance"), inline = T)),
      p(textOutput(ns("nb_points"), inline = T), "GPS points"),
      p("Expected time:", textOutput(ns("time_expected"), inline = T))),
    
    # -- main content
    leafletOutput(ns("map"))
    
  ) # nav_panel
  
}
