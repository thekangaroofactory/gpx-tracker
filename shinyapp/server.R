
# Define server logic required to draw a histogram
function(input, output, session) {
  
  # -- list available files
  gpx_files <- list.files(path = Sys.getenv("DATA_HOME"), pattern = ".gpx")
  
  # -- file selector layout
  output$file_selector <- renderUI(layout_file_selector(files = basename(gpx_files)))
  
  # -- file selector listener
  observeEvent(input$open_track, {
    
    # -- wrap instructions
    withProgress(
      min = 0,
      max = 100,
      value = 10,
      message = "Init",
      
      {
        
        # -- generate module uuid
        # otherwise module would crash when adding another instance
        uuid <- rlang::hash(input$open_track)
        
        # -- start module server
        setProgress(
          value = 10,
          message = "Load & analyze data")
        
        itinary_Server(id = uuid, filename = input$open_track)
        
        # -- build tab ui
        setProgress(
          value = 60,
          message = "Build UI")
        
        content <- layout_itinary(id = uuid)
        
        # -- insert tab
        setProgress(
          value = 90,
          message = "Open tab")
        
        nav_insert(id = "nav",
                   nav = content,
                   select = T)
        
      })
    
  })
  
}
