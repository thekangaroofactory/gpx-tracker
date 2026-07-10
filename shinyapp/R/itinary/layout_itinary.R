

layout_itinary <- function(id, title = "Itinary"){
  
  # -- namespace
  ns <- NS(id)
  
  # -- return UI
  nav_panel(
    title = title,
    value = id,
    icon = icon("route"),
    class = "html-fill-item html-fill-container bslib-gap-spacing",
    
    h1(textOutput(ns("title"))),
    
    layout_column_wrap(
      actionLink(inputId = paste0("close_", id), label = "close",  onclick = 'Shiny.setInputValue(\"close_track\", this.id, {priority: \"event\"})'),
      p(textOutput(ns("nb_day"), inline = T), "itinerary"),
      p("Distance:", textOutput(ns("distance"), inline = T)),
      p(textOutput(ns("nb_points"), inline = T), "GPS points"),
      p("Elapsed time:", textOutput(ns("time_elapsed"), inline = T)),
      p("Activity time:", textOutput(ns("time_activity"), inline = T))),
    
    layout_sidebar(
      border = F,
      
      # -- timeline
      sidebar = uiOutput(ns("timeline")),
      
      # -- main content
      layout_columns(
        col_widths = c(7, 5),
        
        # -- the map
        tagList(
          h4("Track"),
          card(
            leafletOutput(ns("map")))),
        
        # -- the plots
        tagList(
          
          h4("Elevation"),
          card(
            plotOutput(ns("elevation")),
            card_footer(style = "border-top:none;",
                        "Gains:", icon("arrow-up"), textOutput(ns("elevation_up"), inline = T), icon("arrow-down"), textOutput(ns("elevation_down"), inline = T),
                        " | ", icon("caret-down"), textOutput(ns("elevation_lowest"), inline = T), icon("caret-up"), textOutput(ns("elevation_highest"), inline = T))),
          
          h4("Speed"),
          card(
            plotOutput(ns("speed")),
            card_footer(style = "border-top:none;",
                        layout_columns(
                          div(style = "margin: auto;", icon("gauge-high"), textOutput(ns("speed_max"), inline = T)),
                          div(style = "margin: auto;", icon("gauge-simple"), textOutput(ns("speed_average"), inline = T)),
                          div(style = "margin: auto;", icon("gauge"), textOutput(ns("speed_median"), inline = T)))))))))
  
}
