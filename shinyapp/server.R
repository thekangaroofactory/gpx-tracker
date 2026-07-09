
# Define server logic required to draw a histogram
function(input, output, session) {
  
  # -- declare objects
  cache_ids <- reactiveVal()
  
  # -- list available files
  gpx_files <- list.files(path = Sys.getenv("DATA_HOME"), pattern = ".gpx")
  
  # -- file selector layout
  output$file_selector <- renderUI(layout_file_selector(files = basename(gpx_files)))
  
  # -- file selector listener
  observeEvent(input$open_track, {
    
    # -- generate module uuid
    # otherwise module would crash when adding another instance
    uuid <- rlang::hash(input$open_track)
    
    # -- check
    # to avoid tab duplication (& server module crash)
    if(uuid %in% cache_ids())
      
      nav_select(id = "nav", selected = uuid)
    
    else
      
      # -- wrap instructions
      withProgress(
        min = 0,
        max = 100,
        value = 10,
        message = "Init",
        
        {
          
          # -- store
          cache_ids(c(cache_ids(), uuid))
          
          # -- start module server
          setProgress(
            value = 10,
            message = "Load & analyze data")
          
          itinary_Server(id = uuid, filename = input$open_track)
          
          # -- ui
          setProgress(
            value = 60,
            message = "Build UI")
          
          # -- build ui
          content <- layout_itinary(id = uuid, title = gsub("[0-9]|-|_|.gpx", "", input$open_track))
          
          # -- insert tab
          setProgress(
            value = 90,
            message = "Open tab")
          
          nav_insert(id = "nav",
                     nav = content,
                     select = T)
          
        })
    
  })
  
  
  # -- close itinerary (tab)
  observeEvent(input$close_track, {
    
    cat("Close itinary", input$close_track, "\n")
    
    # -- extract id
    track_id <- gsub("close_", "", input$close_track)
    
    # -- drop from cache
    cache_ids(cache_ids()[!cache_ids() %in% track_id])
    
    # -- close
    nav_remove(id = "nav", target = track_id)
    
  })
  
}
