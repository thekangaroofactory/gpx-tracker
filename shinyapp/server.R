
# Define server logic required to draw a histogram
function(input, output, session) {
  
  # -- list available files
  gpx_files <- list.files(path = Sys.getenv("DATA_HOME"), pattern = ".gpx")
  
  # -- file selector layout
  output$file_selector <- renderUI(layout_file_selector(files = basename(gpx_files)))
  
  # -- file selector listener
  observeEvent(input$open_track, {
    
    # -- generate module uuid
    # otherwise module would crash when adding another instance
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

}
