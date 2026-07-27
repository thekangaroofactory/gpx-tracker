

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
    
    # -- objects
    activity <- reactiveVal("default")
    
    
    # --------------------------------------------------------------------------
    # Compute stats
    # --------------------------------------------------------------------------
    
    # -- compute distance
    distance <- sum(segments$distance) / 1000
    
    # -- compute times
    time_elapsed <- difftime(max(segments$datetime_end), min(segments$datetime_start), units = "hours")
    nb_day <- trunc(distance / c(LEG_DISTANCE_MAX, LEG_DISTANCE_MIN))

    
    # --------------------------------------------------------------------------
    # Manage legs
    # --------------------------------------------------------------------------
    
    # -- read leg file
    legs_full <- read_track_data(type = "leg")
    legs_init <- legs_full |> filter(track_id == id)
    legs <- reactiveVal(legs_init)
    
    # -- add popup & label
    if(!is.null(legs_init)){
      legs_init <- legs_init |> 
        mk_popup(info = c("title", "show_targets", "remove_leg", "cum_distance", "distance"), ns = ns) |>
        mutate(type = "leg",
               label = "Leg")}
    
    # -- persistence
    # popup & label are not saved
    observeEvent(legs(),
                 iker::write_data(bind_rows(legs_full |> filter(track_id != id), legs()), file = "legs.csv"),
                 ignoreInit = TRUE)

    # -- display leg targets
    observeEvent(input$leg_targets, {

      # -- extract input value (reference segment id)
      ref_id <- split_input(input$leg_targets)['value']
      
      # -- compute targets
      targets <- segments |> leg_targets(start = ref_id, min = LEG_DISTANCE_MIN, max = LEG_DISTANCE_MAX, step = LEG_DISTANCE_STEP)
      targets <- targets |> mk_popup(c("title", "clear_targets"), ns = ns) |> mutate(label = paste(round(distance, digits = 0), "km"))
      bounds <- bounding_box(points = targets)

      # -- update map
      leafletProxy("map", session) |>
        
        # -- clear group (to avoid multiple markers)
        clearGroup("leg_target") |>

        # -- add markers
        m_marker(markers = targets, group = "leg_target") |>
        
        # -- zoom
        m_fly_bounds(bounds)
      
      # -- activate leg mode
      toggle_switch(id = "leg_mode", value = TRUE)
        
    })
    
    
    # -- create leg
    observeEvent(input$create_leg, {
      
      # -- extract segment id
      leg_id <- split_input(input$create_leg)['value']
      
      # -- extract leg point
      leg <- segments |> 
        leg_targets(start = leg_id, min = 0, max = 0) |> 
        mutate(track_id = id,
               type = "leg")
        
      # -- update distance
      # distance out of leg_targets() will be 0
      leg_table <- bind_rows(legs(), leg) |>
        arrange(segment_id) |>
        mutate(distance = cum_distance - lag(cum_distance)) |>
        mutate(distance = replace_when(distance, is.na(distance) ~ cum_distance))
      
      # -- store updated table
      legs(leg_table)
      
      # -- add popup & label
      # take legs from the table to get updated distance
      # new leg + next leg (since distance has changed for this point)
      leg <- leg_table |>
        filter(segment_id == leg_id |
               lag(segment_id) == leg_id) |>
        mk_popup(info = c("title", "show_targets", "remove_leg", "cum_distance", "distance"), ns = ns) |>
        mutate(type = "leg",
               label = "Leg")
      
      # -- update map
      leafletProxy("map", session) |>
        
        # -- cleanup targets & click
        clearGroup(group = "leg_target") |>
        removeMarker(layerId = c("click", paste0("leg_", tail(leg$segment_id, n = 1L))))|>
      
        # -- add markers (new + next legs)
        m_marker(leg, layerId = paste0("leg_", leg$segment_id), group = "legs")
      
    }, ignoreInit = TRUE)
    
    
    # -- remove leg
    observeEvent(input$drop_leg, {
      
      # -- extract input value (reference segment id)
      leg_id <- split_input(input$drop_leg)['value']
      
      # -- drop from leg table
      legs(legs() |> filter(segment_id != leg_id))
      
      # -- update map (cleanup leg marker)
      leafletProxy("map", session) |>
        removeMarker(layerId = paste0("leg_", leg_id))
      
    }, ignoreInit = TRUE)
    
    
    # --------------------------------------------------------------------------
    # Outputs
    # --------------------------------------------------------------------------
    
    warning("Check comment factoriser ça avec l'autre serveur!")
    
    # -- title
    output$title <- renderText(title)
    
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
    
    # -- track bounding box
    track_bounds <- bounding_box(segments = segments)
    
    # -- baseline map: tile + track
    map_track <- m_track(segments)
    
    # -- add track bounds
    start <- segments |> bound_start() |> mk_popup(info = c("title", "show_targets", "distance"), ns = ns)
    finish <- segments |> bound_finish() |> mk_popup(info = c("title", "distance"))
    map_track <- map_track |> m_marker(markers = bind_rows(start, finish))
    
    # -- add legs (if any)
    # need to rebuilt popup & label
    if(!is.null(legs_init))
      map_track <- map_track |> m_marker(legs_init, layerId = paste0("leg_", legs_init$segment_id), group = "legs")
    
    # -- the map
    output$map <- renderLeaflet(map_track)
    
    # -- activate leg mode (activity)
    observeEvent(input$leg_mode, 
                 
                 if(input$leg_mode) 
                   activity("leg") 
                 else {
                   # -- update map
                   # (in case a click or target markers are displayed)
                   leafletProxy("map", session) |>
                     removeMarker(layerId = "click") |>
                     clearGroup(group = "leg_target")
                   activity("default")})
    
    # -- map click listener
    observeEvent(input$map_click, {
      
      # -- leg mode
      if(activity() == "leg"){
        
        # -- get nearest segment
        idx <- nearest_index(segments, lng = input$map_click$lng, lat = input$map_click$lat)
        x <- segments |> 
          filter(segment_id == idx)|>
          mutate(lng = lng_end,
                 lat = lat_end,
                 elevation = elevation_end,
                 type = "click",
                 label = paste(round(cum_distance, digits = 0), "km")) |>
          select(segment_id, lng, lat, elevation, cum_distance, type, label) |>
          mk_popup(info = c("title", "add_leg"), ns = ns)
        
        # -- update map
        leafletProxy("map", session) |>
          
          # -- cleanup previous marker
          removeMarker(layerId = "click") |>
          
          # -- add targets
          m_marker(x, layerId = "click")
        
      }
      
    })
    
    # -- zoom (fit track)
    observeEvent(input$map_fit, {
      
      # -- update map
      leafletProxy("map", session) |>
        m_fly_bounds(bounds = track_bounds)
      
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
