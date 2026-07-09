

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
    
    
    
  
)
