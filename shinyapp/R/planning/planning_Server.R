

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

planning_Server <- function(id, segments, title) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # -- namespace
    ns <- session$ns
    
    
    # --------------------------------------------------------------------------
    # Compute stats
    # --------------------------------------------------------------------------
    
    # -- compute distance
    distance <- sum(segments$distance) / 1000
    
    # -- compute times
    time_elapsed <- difftime(max(segments$datetime_end), min(segments$datetime_start), units = "hours")
    nb_day <- trunc(distance / c(LEG_DISTANCE_MAX, LEG_DISTANCE_MIN))
    
    # -- base map
    m_baseline <- m_track(segments)
    
    
    # --------------------------------------------------------------------------
    # Manage legs
    # --------------------------------------------------------------------------

    legs <- readr::read_csv(file = file.path(Sys.getenv("DATA_HOME"), "legs.csv"))
    
    observeEvent(input$add_leg, {

      # -- compute targets
      targets <- segments |> filter(segment_id %in% leg_targets(segments, min = LEG_DISTANCE_MIN, max = LEG_DISTANCE_MAX, step = LEG_DISTANCE_STEP))
      bounds <- bounding_box(targets)
      
      # -- update map
      leafletProxy("map", session) |>
        
        # -- add targets
        addMarkers(data = targets,
                   lng = ~st_coordinates(geometry_start)[,1],
                   lat = ~st_coordinates(geometry_start)[,2],
                   label = ~paste(round(cum_distance, digits = 0), "km")) |>
        
        # -- zoom
        flyToBounds(lng1 = bounds[['lng1']],
                    lat1 = bounds[['lat1']],
                    lng2 = bounds[['lng2']],
                    lat2 = bounds[['lat2']])
        
    })
    
    
    # --------------------------------------------------------------------------
    # Outputs
    # --------------------------------------------------------------------------
    
    warning("Check comment factoriser ça avec l'autre serveur!")
    
    # -- title
    output$title <- renderText(tail(unlist(strsplit(unlist(strsplit(title, split = ".", fixed = T))[1], "_")), 1))
    
    # -- GPS points
    output$nb_points <- renderText(nrow(segments) + 1)
    
    # -- times
    output$time_expected <- renderText(paste0(floor(time_elapsed), "h", floor((time_elapsed - floor(time_elapsed)) * 60), "min"))
    
    
    output$nb_day <- renderText(paste0(paste(nb_day, collapse = "/"), "-day"))
    
    # -- distance
    output$distance <- renderText(paste0(round(distance, digits = 1), "km"))
    
    
    # --------------------------------------------------------------------------
    # Map
    # --------------------------------------------------------------------------
    
    # -- track map
    output$map <- renderLeaflet(m_baseline)
    
    observeEvent(input$map_click, {
      
      str(input$map_click)
      
    })
    
    
    
    
    
    
    
    # -- return
    NULL
    
  })
}
