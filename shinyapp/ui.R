

# Define UI for application that draws a histogram
tagList(
  
  # -- keep it outside
  shinyjs::useShinyjs(),
  
  # -- actual UI
  page_navbar(
    
    # -- parameters
    title = "GPX-Tracker",
    window_title = "GPX-Tracker",
    id = "nav",
    footer = p(style = "font-size:9pt;margin-left:20px;", paste("©", format(Sys.Date(), "%Y"), "Philippe Peret", "|", paste0("Version", app_version))),
    
    # -- custom CSS
    header = tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/slider.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/gpx_tracker.css")),
    
    # -- theme
    theme = bs_theme(
      bg = "#e8e5d4",
      fg = "#000",
      base_font = font_google("Quicksand")),
    
    # -- content
    # by default, the ui contains only the home tab
    nav_panel(
      title = "Home",
      value = "home",
      icon = icon("home"),
      
      h1("Tracks"),
      p("Available GPX files:"),
      uiOutput("file_selector"))
    
  ))
