

# Define UI for application that draws a histogram
page_navbar(
  
  # -- parameters
  title = "GPX-Tracker",
  window_title = "GPX-Tracker",
  id = "nav",
  footer = p(style = "font-size:9pt;margin-left:20px;", paste("©", format(Sys.Date(), "%Y"), "Philippe Peret", "|", "Version v0.9")),
  
  # -- theme
  theme = bs_theme(
    bg = "#e8e5d4",
    fg = "#000",
    base_font = font_google("Quicksand")),
  
  # -- content
  nav_panel(
    title = "Home",
    value = "home",
    icon = icon("home"),
    
    h1(textOutput("title")),
    
    layout_column_wrap(
      p("Distance:", textOutput("distance", inline = T)),
      p(textOutput("nb_points", inline = T), "GPS points"),
      p("Elapsed time:", textOutput("time_elapsed", inline = T))),
    
    layout_columns(
      col_widths = c(6,6),
      leafletOutput("map"),
      tagList(
        
        h4("Elevation"),
        card(
          plotOutput("elevation"),
          card_footer(style = "border-top:none;",
                      "Gains:", icon("arrow-up"), textOutput("elevation_up", inline = T), icon("arrow-down"), textOutput("elevation_down", inline = T),
                      " | ", icon("caret-down"), textOutput("elevation_lowest", inline = T), icon("caret-up"), textOutput("elevation_highest", inline = T))),
        
        h4("Speed"),
        card(
          plotOutput("speed"),
          card_footer(style = "border-top:none;", 
                      icon("gauge-high"), textOutput("speed_max", inline = T), 
                      icon("gauge"), textOutput("speed_mean", inline = T), 
                      textOutput("speed_median", inline = T)))))
    
  )
  
)
