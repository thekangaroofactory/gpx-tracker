

milestones_summary <- function(track){
  
  # -- start & finish points
  start <- track |> bound_start()
  finish <- track |> bound_finish()
  
  # -- breaks
  breaks <- break_summary(track)

  # -- merge
  milestones <- bind_rows(start, finish, breaks)
  
  # -- get address info
  # zoom = 10 for city level API output
  # milestones$city <- unlist(lapply(milestones$lng_start, function(geometry){
  # 
  #   warning("*** rework since we cant loop on geometry anymore (apply over row number)")
  #   # >>> also no need to call on short & medium breaks (since it's now computed inside this function)
  #   
  #   # -- call API
  #   # osm_data <- reverse_geocoding(lng = st_coordinates(geometry)[, 1], 
  #   #                               lat = st_coordinates(geometry)[, 2],
  #   #                               zoom = 10)
  #   osm_data <- list()
  #   
  #   # -- return (depending on result type)
  #   if(length(osm_data))
  #     osm_data$address[osm_data$addresstype]
  #   else NA
  #   
  # }))
  milestones$city <- NA
  
  milestones |>
    arrange(segment_id)
    
}
