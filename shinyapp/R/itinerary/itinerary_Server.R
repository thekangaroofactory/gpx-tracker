

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

itinerary_Server <- function(id, segments, filename) {
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
    # Compute Data Summaries
    # --------------------------------------------------------------------------
    
    setProgress(
      value = 20,
      message = "Compute summaries")
    
    # -- compute distance
    distance <- sum(segments$distance) / 1000
    
    # -- compute summaries
    elevation <- elevation_summary(segments)
    breaks <- break_summary(segments)
    milestones <- milestones_summary(segments, breaks)
    distances <- distance_summary(segments, dist = ifelse(distance >= 50, 10, 5), overnight = milestones |> filter(type == "overnight"))
    
    
    # --------------------------------------------------------------------------
    # Debug
    # --------------------------------------------------------------------------

    # -- debug
    if(DEBUG){
      debug_segments <<- segments
      debug_milestones <<- milestones
      debug_breaks <<- breaks
      debug_distances <<- distances
    }
    
    
    # --------------------------------------------------------------------------
    # Compute Stats
    # --------------------------------------------------------------------------
    
    setProgress(
      value = 40,
      message = "Compute stats")
    
    # -- times
    time_elapsed <- difftime(max(segments$datetime_end), min(segments$datetime_start), units = "hours")
    time_activity <- time_elapsed - (sum(segments[segments$time > 20, ]$time) / 3600)
    nb_day <- as.Date(max(segments$datetime_end)) - as.Date(min(segments$datetime_start)) + 1
    
    # -- speed
    speed_average <- round(distance / as.numeric(time_activity), digits = 1)
    
    
    # --------------------------------------------------------------------------
    # Outputs
    # --------------------------------------------------------------------------
    
    setProgress(
      value = 50,
      message = "Build map & plots")
    
    # -- title
    output$title <- renderText(tail(unlist(strsplit(unlist(strsplit(filename, split = ".", fixed = T))[1], "_")), 1))
    
    # -- GPS points
    output$nb_points <- renderText(nrow(segments) + 1)
    
    # -- times
    output$time_elapsed <- renderText(paste0(floor(time_elapsed), "h", floor((time_elapsed - floor(time_elapsed)) * 60), "min"))
    output$time_activity <- renderText(paste0(floor(time_activity), "h", floor((time_activity - floor(time_activity)) * 60), "min"))
    output$nb_day <- renderText(paste0(nb_day, "-day"))
    
    # -- distance
    output$distance <- renderText(paste0(round(distance, digits = 1), "km"))
    
    
    # --------------------------------------------------------------------------
    # Timeline & map
    # --------------------------------------------------------------------------

    # -- milestones
    output$timeline <- renderUI(timeline(milestones))
    
    # -- track map
    # saved as an object for reuse purpose
    map_track <- m_track(segments, breaks)
    output$map <- renderLeaflet(map_track)
    
    
    # --------------------------------------------------------------------------
    # Elevation
    # --------------------------------------------------------------------------

    # -- elevation stats
    output$elevation_up <- renderText(paste0(round(elevation['pos_gain'], digits = 0), "m"))
    output$elevation_down <- renderText(paste0(round(elevation['neg_gain'], digits = 0), "m"))
    output$elevation_lowest <- renderText(paste0(round(elevation['lowest'], digits = 0), "m"))
    output$elevation_highest <- renderText(paste0(round(elevation['highest'], digits = 0), "m"))
    
    # -- elevation profile
    output$elevation <- renderPlot(p_elevation(segments), bg = "transparent")
    
    
    # --------------------------------------------------------------------------
    # Speed
    # --------------------------------------------------------------------------
    
    # -- speed stats
    output$speed_max <- renderText(paste0(round(max(segments$speed, na.rm = T), digits = 1), "km/h"))
    output$speed_average <- renderText(paste0(speed_average, "km/h"))
    output$speed_median <- renderText(paste0(round(median(segments$speed, na.rm = T), digits = 1), "km/h"))
    
    # -- speed profile
    output$speed <- renderPlot(p_speed(segments), bg = "transparent")
    
    
    # --------------------------------------------------------------------------
    # Progress
    # --------------------------------------------------------------------------

    # -- progress plot
    output$distance_ruler <- renderPlot(p_distance_ruler(distances, overnight = milestones |> filter(type == "overnight")), bg = "transparent")

    
    # ***************** 
    # this section will go into the compute phase on top
    
    
    output$section_slow <- renderLeaflet(m_section(map_track, section = section(segments, distances)))
    output$section_fast <- renderLeaflet(m_section(map_track, 
                                                   section = section(segments, distances, type = "fast"),
                                                   type = "fast"))
    
    
    # --------------------------------------------------------------------------
    # Slider
    # --------------------------------------------------------------------------

    # -- listener (switch slides)
    # also return value so that calling code can destroy it.
    obs <- layout_slider_listener(id, input)
    
  })
}
