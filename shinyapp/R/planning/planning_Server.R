

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
    
    # -- bounds
    bounds <- segments |> track_bounds() |> popup_label() |> popup_leg(ns = ns)
    m_baseline <- m_baseline |> m_start_finish(bounds)
    
    
    # --------------------------------------------------------------------------
    # Manage legs
    # --------------------------------------------------------------------------

    # -- read leg file
    legs <- reactiveVal(readr::read_csv(file = file.path(Sys.getenv("DATA_HOME"), "legs.csv")))
    
    # -- button listener (to replace)
    observeEvent(input$init_leg, {

      # -- extract input value (reference segment id)
      ref_id <- split_input(input$init_leg)['value']
      ref_distance <- if(ref_id == 1) 0 else segments |> filter(segment_id == ref_id) |> pull(cum_distance)
      
      # -- compute targets
      targets <- segments |> filter(segment_id %in% leg_targets(segments, start = ref_id, min = LEG_DISTANCE_MIN, max = LEG_DISTANCE_MAX, step = LEG_DISTANCE_STEP))
      targets <- targets |> popup_target(ns = ns)
      bounds <- bounding_box(targets)
      
      # -- icon
      i_leg_target <- makeAwesomeIcon(
        icon = "location-crosshairs",
        library = "fa",
        markerColor = "lightgray")
      
      # -- update map
      leafletProxy("map", session) |>
        
        # -- clear group (to avoid multiple markers)
        clearGroup("leg_target") |>
        
        # -- add targets
        addAwesomeMarkers(data = targets,
                          lng = ~st_coordinates(geometry_end)[,1],
                          lat = ~st_coordinates(geometry_end)[,2],
                          group = "leg_target",
                          icon = i_leg_target,
                          label = ~paste(round(cum_distance - ref_distance, digits = 0), "km"),
                          popup = ~popup) |>
        
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
    
    # -- the map
    output$map <- renderLeaflet(m_baseline)
    
    # -- map click listener
    observeEvent(input$map_click, {
      
      # -- get nearest segment
      idx <- nearest_index(segments, lng = input$map_click$lng, lat = input$map_click$lat)
      x <- segments |> filter(segment_id == idx)
      
      # -- update map
      leafletProxy("map", session) |>
      
        # -- cleanup previous marker
        removeMarker(layerId = "click") |>
      
        # -- add targets
        addMarkers(data = x,
                   lng = ~st_coordinates(geometry_end)[,1],
                   lat = ~st_coordinates(geometry_end)[,2],
                   layerId = "click",
                   popup = paste(actionLink(inputId = ns(paste0("add_leg_", idx)), 
                                      label = "Add leg", 
                                      onclick = paste0('Shiny.setInputValue(\"', ns("create_leg"), '\", this.id, {priority: \"event\"})'))),
                   label = ~paste(round(cum_distance, digits = 0), "km"))
      
    })
    
    
    # -- actionLink listener
    observeEvent(input$create_leg, {
      
      # -- extract leg
      leg <- segments |> 
        filter(segment_id == split_input(input$create_leg)['value']) |>
        mutate(label = "Leg")
      
      leg <- leg |> popup_label() |> popup_leg(ns = ns)
      
      # -- store new leg
      legs(bind_rows(legs(), leg))
      
      # -- declare icons
      i_leg_finish <- makeAwesomeIcon(
        icon = "flag",
        library = "fa",
        markerColor = "beige")
      
      # -- update map
      leafletProxy("map", session) |>
        
        # -- cleanup previous marker
        removeMarker(layerId = "click") |>
        clearGroup(group = "leg_target") |>
        
        # -- add leg finish
        addAwesomeMarkers(data = leg,
                          lng = ~st_coordinates(geometry_end)[,1],
                          lat = ~st_coordinates(geometry_end)[,2],
                          icon = i_leg_finish,
                          popup = ~popup,
                          group = "legs",
                          label = ~paste(round(cum_distance, digits = 0), "km"))
      
      
    }, ignoreInit = TRUE)
    
    
    # -- clear group
    observeEvent(input$clear_group, {
      
      # -- extract group name from input
      group <- split_input(input$clear_group)[['action']]
      
      # -- update map
      leafletProxy("map", session) |>
        clearGroup(group)
      
    })
    
    
    # -- return
    NULL
    
  })
}
