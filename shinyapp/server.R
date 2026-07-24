
# Define server logic required to draw a histogram
function(input, output, session) {
  
  # -- declare objects
  cache_ids <- reactiveVal()
  cache_obs <- reactiveVal()
  
  # -- list available files
  gpx_files <- list.files(path = Sys.getenv("DATA_HOME"), pattern = ".gpx", recursive = TRUE)
  
  # -- file selector layout
  output$file_selector_done <- renderUI(layout_file_selector(files = gpx_files[!grepl("planned", gpx_files)]))
  output$file_selector_planned <- renderUI(layout_file_selector(files = gpx_files[grepl("planned", gpx_files)]))
  
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
        value = 0,
        message = "Init",
        
        {
          
          # -- store
          cache_ids(c(cache_ids(), uuid))
          
          # -- load GPX file, compute segments & stats
          setProgress(
            value = 5,
            message = "Load GPX data")
          
          track_segments <- read_gpx(file.path(Sys.getenv("DATA_HOME"), input$open_track)) |>
            pts_to_seg() |>
            seg_stats()
          
          # -- start module server
          # return value is stored to destroy observer later
          setProgress(
            value = 10,
            message = "Launch track module")
          
          obs <- itinerary_Server(id = uuid, segments = track_segments, filename = input$open_track)
          cache_obs(obs)
          
          # -- build ui
          setProgress(
            value = 80,
            message = "Build UI")
          
          content <- layout_itinerary(id = uuid, title = gsub("[0-9]|-|_|.gpx", "", input$open_track))
          
          # -- insert tab
          setProgress(
            value = 100,
            message = "Open tab")
          
          nav_insert(id = "nav",
                     nav = content,
                     select = T)
          
        })
    
  })
  
  
  # -- close itinerary (tab)
  observeEvent(input$close_track, {
    
    cat("Close itinerary", input$close_track, "\n")
    
    # -- extract id
    track_id <- gsub("close_", "", input$close_track)
    
    # -- drop from cache
    cache_ids(cache_ids()[!cache_ids() %in% track_id])
    
    # -- close nav
    nav_select(id = "nav", selected = "home")
    nav_remove(id = "nav", target = track_id)
    
    # -- destroy module listener & inputs
    cache_obs()$destroy()
    cleanup_inputs(id = track_id, input)
    session$userData[[NS(track_id, "slider_active")]] <- NULL
    
  })
  
}
