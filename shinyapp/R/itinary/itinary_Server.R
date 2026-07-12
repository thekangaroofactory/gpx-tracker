

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

itinary_Server <- function(id, filename) {
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
    # Init
    # --------------------------------------------------------------------------
    
    # -- load GPX file
    track <- read_gpx(file.path(Sys.getenv("DATA_HOME"), filename))
    
    # -- compute segments & stats
    track_segments <- track |>
      pts_to_seg() |>
      seg_stats()
    
    
    # --------------------------------------------------------------------------
    # Computation & Summaries
    # --------------------------------------------------------------------------
    
    # -- compute summaries
    elevation <- elevation_summary(track_segments)
    breaks <- break_summary(track_segments)
    milestones <- milestones_summary(track_segments, breaks)
    
    # -- compute distance
    distance <- sum(track_segments$distance) / 1000
    
    # -- compute times
    time_elapsed <- difftime(max(track$datetime), min(track$datetime), units = "hours")
    time_activity <- time_elapsed - (sum(track_segments[track_segments$time > 20, ]$time) / 3600)
    nb_day <- as.Date(max(track$datetime)) - as.Date(min(track$datetime)) + 1
    
    
    # --------------------------------------------------------------------------
    # Debug
    # --------------------------------------------------------------------------

    # -- debug
    debug_track_segments <<- track_segments
    debug_milestones <<- milestones
    debug_breaks <<- breaks
    
    
    # ----------------------------------------------------------------------------
    # Outputs
    # ----------------------------------------------------------------------------
    
    # -- itinary title
    output$title <- renderText(tail(unlist(strsplit(unlist(strsplit(filename, split = ".", fixed = T))[1], "_")), 1))
    
    # -- GPS points
    output$nb_points <- renderText(nrow(track))
    
    # -- times
    output$time_elapsed <- renderText(paste0(floor(time_elapsed), "h", floor((time_elapsed - floor(time_elapsed)) * 60), "min"))
    output$time_activity <- renderText(paste0(floor(time_activity), "h", floor((time_activity - floor(time_activity)) * 60), "min"))
    output$nb_day <- renderText(paste0(nb_day, "-day"))
    
    # -- distance
    output$distance <- renderText(paste0(round(distance, digits = 1), "km"))

    # -- milestone
    output$timeline <- renderUI(timeline(milestones))
    
    # -- track map
    output$map <- renderLeaflet(m_track(track_segments, breaks))

    # -- elevation
    output$elevation_up <- renderText(paste0(round(elevation['pos_gain'], digits = 0), "m"))
    output$elevation_down <- renderText(paste0(round(elevation['neg_gain'], digits = 0), "m"))
    output$elevation_lowest <- renderText(paste0(round(elevation['lowest'], digits = 0), "m"))
    output$elevation_highest <- renderText(paste0(round(elevation['highest'], digits = 0), "m"))
    
    # -- elevation profile
    output$elevation <- renderPlot(p_elevation(track_segments), bg = "transparent")
    
    # -- speed
    output$speed_max <- renderText(paste0(round(max(track_segments$speed, na.rm = T), digits = 1), "km/h"))
    output$speed_average <- renderText(paste0(round(distance / as.numeric(time_activity), digits = 1), "km/h"))
    output$speed_median <- renderText(paste0(round(median(track_segments$speed, na.rm = T), digits = 1), "km/h"))
    
    # -- speed profile
    output$speed <- renderPlot(p_speed(track_segments), bg = "transparent")
    
    
    # ----------------------------------------------------------------------------
    # Slider
    # ----------------------------------------------------------------------------
    
    # -- slider listener
    # event on toggles & dots
    observeEvent(input$slider_event, {
      
      # -- extract target slide nb
      target_slide <- gsub("toggle_", "", input$slider_event) |> gsub(pattern = "dot_", replacement = "")
      
      # -- close active slide
      shinyjs::toggleClass(selector = '.slider-active', class = 'slider-active')
      shinyjs::toggleClass(selector = '.slider-toggle-active', class = 'slider-toggle-active')
      shinyjs::toggleClass(selector = '.slider-dot-on', class = 'slider-dot-on')
      
      # -- open target slide
      shinyjs::toggleClass(selector = paste0('#slide-', target_slide), class = 'slider-active')
      shinyjs::toggleClass(selector = paste0('#toggle_', target_slide), class = 'slider-toggle-active')
      shinyjs::toggleClass(selector = paste0('#dot_', target_slide), class = 'slider-dot-on')
      
    })
    
    
  })
}
