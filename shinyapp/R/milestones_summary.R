

milestones_summary <- function(track, breaks){
  
  # -- starting point
  start <- track |>
    filter(row_number() == 1) |>
    mutate(type = "start")
  
  # -- finish point
  finish <- track |>
    filter(row_number() == n()) |>
    mutate(type = "finish")
  
  # -- major breaks
  breaks <- breaks |> 
    filter(type  %in% c("long", "overnight"))

  # -- merge
  milestones <- rbind(start, finish, breaks)
  
  # -- get address info
  # zoom = 10 for city level API output
  milestones$city <- unlist(lapply(milestones$geometry_start, function(geometry){
  
    # -- call API
    # osm_data <- reverse_geocoding(lng = st_coordinates(geometry)[, 1], 
    #                               lat = st_coordinates(geometry)[, 2],
    #                               zoom = 10)
    osm_data <- list()
    
    # -- return (depending on result type)
    if(length(osm_data))
      osm_data$address[osm_data$addresstype]
    else NULL
    
  }))
  
  milestones |>
    select(-c(slope, elevation_gain, cum_speed, speed, distance, time)) |>
    arrange(segment_id)
    
}
