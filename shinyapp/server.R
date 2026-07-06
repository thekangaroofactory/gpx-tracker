
# Define server logic required to draw a histogram
function(input, output, session) {
  
  # -- load data
  filename <- "2026-06-20_3055267978_Charenton à Troyes.gpx"
  track <- read_gpx(file.path(Sys.getenv("DATA_HOME"), filename))
  
  # -- compute segments & stats
  track_segments <- track |>
    pts_to_seg() |>
    seg_stats()
  
  elevation <- elevation_summary(track_segments)
  
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
  
  
  output$map <- renderLeaflet(
    leaflet(data = track) %>%
      addTiles() %>%
      addPolylines(lng = ~st_coordinates(geometry)[,1], lat = ~st_coordinates(geometry)[,2], weight = 2, color = "black", opacity = 1) %>%
      addCircleMarkers(data = track_segments |> filter(time > 60), lng = ~st_coordinates(geometry_start)[,1], lat = ~st_coordinates(geometry_start)[,2], radius = ~time/1000))
  
  
  # -- elevation profile
  output$elevation <- renderPlot(p_elevation(track_segments), bg = "transparent")
  
  # -- speed profile
  output$speed <- renderPlot(
    ggplot(track_segments, aes(x = segment_id, y = speed)) +
      geom_area() +
      plot_theme(),
    bg = "transparent")
  
  
}
