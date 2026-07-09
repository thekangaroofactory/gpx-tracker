
# Define server logic required to draw a histogram
function(input, output, session) {
  
  # -- check available files
  gpx_files <- list.files(path = Sys.getenv("DATA_HOME"), pattern = ".gpx")
  
  output$file_selector <- renderUI(
    
    layout_column_wrap(
      !!!lapply(basename(gpx_files), function(x) {
    
    card(
      card_header("Itinary"),
      x,
      card_footer(actionLink(inputId = x, 
                             label = "open",
                             onclick = 'Shiny.setInputValue(\"open_track\", this.id, {priority: \"event\"})')))
    
  })))
  
  observeEvent(input$open_track, {
    
    # -- generate module uuid
    uuid <- rlang::hash(input$open_track)
    
    # -- start module server
    itinary_Server(id = uuid, filename = input$open_track)
    
    # -- build tab ui
    content <- layout_itinary(id = uuid)
    
    # -- insert tab
    nav_insert(id = "nav",
               nav = content,
               select = T)
    
  })
  
  
  # ----------------------------------------------------------------------------
  
}
