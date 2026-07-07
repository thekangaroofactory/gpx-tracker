
# Define server logic required to draw a histogram
function(input, output, session) {
  
  # -- load data
  filename <- "2026-06-20_3055267978_Charenton à Troyes.gpx"
  track <- read_gpx(file.path(Sys.getenv("DATA_HOME"), filename))
  
  # -- compute segments & stats
  track_segments <- track |>
    pts_to_seg() |>
    seg_stats()
  
  # -- debug
  debug_track_segments <<- track_segments
  
  
  elevation <- elevation_summary(track_segments)
  breaks <- break_summary(track_segments)
  
  
  # ----------------------------------------------------------------------------
  # Outputs
  # ----------------------------------------------------------------------------
  
  output$title <- renderText(tail(unlist(strsplit(unlist(strsplit(filename, split = ".", fixed = T))[1], "_")), 1))
  
  output$nb_points <- renderText(nrow(track))
  
  time_elapsed <- difftime(max(track$datetime), min(track$datetime), units = "hours")
  output$time_elapsed <- renderText(paste0(floor(time_elapsed), "h", floor((time_elapsed - floor(time_elapsed)) * 60), "min"))

  output$distance <- renderText(paste0(round(sum(track_segments$distance) / 1000, digits = 1), "km"))
  
  # -- elevation
  output$elevation_up <- renderText(paste0(round(elevation['pos_gain'], digits = 0), "m"))
  output$elevation_down <- renderText(paste0(round(elevation['neg_gain'], digits = 0), "m"))
  output$elevation_lowest <- renderText(paste0(round(elevation['lowest'], digits = 0), "m"))
  output$elevation_highest <- renderText(paste0(round(elevation['highest'], digits = 0), "m"))
  
  # -- elevation profile
  output$elevation <- renderPlot(p_elevation(track_segments), bg = "transparent")
  
  
  # -- track map
  output$map <- renderLeaflet(m_track(track_segments, breaks))
  
  
  # -- speed profile
  output$speed <- renderPlot(p_speed(track_segments), bg = "transparent")
  output$speed_max <- renderText(paste0(round(max(track_segments$speed, na.rm = T), digits = 1), "km/h"))
  output$speed_mean <- renderText(paste0(round(mean(track_segments$speed, na.rm = T), digits = 1), "km/h"))
  output$speed_median <- renderText(paste0(round(median(track_segments$speed, na.rm = T), digits = 1), "km/h"))
  
}
