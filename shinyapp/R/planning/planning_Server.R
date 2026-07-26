

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

    
    # --------------------------------------------------------------------------
    # Manage legs
    # --------------------------------------------------------------------------

    # -- read leg file
    legs <- reactiveVal(readr::read_csv(file = file.path(Sys.getenv("DATA_HOME"), "legs.csv")))
    
    # -- display leg targets
    observeEvent(input$leg_targets, {

      # -- extract input value (reference segment id)
      ref_id <- split_input(input$leg_targets)['value']
      
      # -- compute targets
      targets <- segments |> leg_targets(start = ref_id, min = LEG_DISTANCE_MIN, max = LEG_DISTANCE_MAX, step = LEG_DISTANCE_STEP)
      targets <- targets |> mk_popup(c("title", "clear_targets"), ns = ns) |> mutate(label = paste(round(distance, digits = 0), "km"))
      bounds <- bounding_box(targets)

      # -- update map
      leafletProxy("map", session) |>
        
        # -- clear group (to avoid multiple markers)
        clearGroup("leg_target") |>

        # -- add markers
        m_marker(markers = targets, group = "leg_target") |>
        
        # -- zoom
        flyToBounds(lng1 = bounds[['lng1']],
                    lat1 = bounds[['lat1']],
                    lng2 = bounds[['lng2']],
                    lat2 = bounds[['lat2']])
        
    })
    
    
    # -- create leg
    observeEvent(input$create_leg, {
      
      # -- extract leg
      leg <- segments |> 
        leg_targets(start = split_input(input$create_leg)['value'], min = 0, max = 0) |> 
        mk_popup(info = c("title", "show_targets"), ns = ns) |>
        mutate(type = "leg",
               label = paste(round(cum_distance, digits = 0), "km"))
      
      # -- store new leg
      legs(bind_rows(legs(), leg))
      
      # -- update map
      leafletProxy("map", session) |>
        
        # -- cleanup previous marker
        removeMarker(layerId = "click") |>
        clearGroup(group = "leg_target") |>
      
        # -- add markers
        m_marker(leg, group = "legs")
      
      
    }, ignoreInit = TRUE)
    
    
    # -- remove leg
    observeEvent(input$drop_leg, {
      
      # -- extract input value (reference segment id)
      leg_id <- split_input(input$drop_leg)['value']
      
      # -- drop from leg table
      # legs()
      
      # -- update map
      leafletProxy("map", session) |>
        
        # -- cleanup previous marker
        removeMarker(layerId = "foo")
      
    }, ignoreInit = TRUE)
    
    
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
    
    # -- baseline map: tile + track
    map_track <- m_track(segments)
    
    # -- add track bounds
    start <- segments |> bound_start() |> mk_popup(info = c("title", "show_targets"), ns = ns)
    finish <- segments |> bound_finish() |> mk_popup(info = c("title"))
    map_track <- map_track |> m_marker(markers = bind_rows(start, finish))
    
    # -- the map
    output$map <- renderLeaflet(map_track)
    
    # -- map click listener
    observeEvent(input$map_click, {
      
      # -- get nearest segment
      idx <- nearest_index(segments, lng = input$map_click$lng, lat = input$map_click$lat)
      x <- segments |> 
        filter(segment_id == idx)|>
        mutate(geometry = geometry_end,
               elevation = elevation_end,
               type = "click",
               label = paste(round(cum_distance, digits = 0), "km")) |>
        select(segment_id, geometry, elevation, cum_distance, type, label) |>
        mk_popup(info = c("title", "add_leg"), ns = ns)
      
      # -- update map
      leafletProxy("map", session) |>
      
        # -- cleanup previous marker
        removeMarker(layerId = "click") |>
      
        # -- add targets
        m_marker(x, layerId = "click")
      
    })
    
    
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
