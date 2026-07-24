

m_anomalies <- function(map, speed = NULL, distance = NULL, start = NULL){
  
  # -- declare icons
  i_anomaly <- makeAwesomeIcon(
    icon = "triangle-exclamation",
    library = "fa",
    markerColor = "lightred")

  # -- speed layer
  if(nrow(speed) > 0){
    map <- map |>
      addAwesomeMarkers(data = speed,
                        lng = ~(st_coordinates(geometry_start)[,1] + st_coordinates(geometry_end)[,1])/2,
                        lat = ~(st_coordinates(geometry_start)[,2] + st_coordinates(geometry_end)[,2])/2,
                        label = "Segment anomaly (speed)",
                        icon = i_anomaly)}
  
  # -- distance layer
  if(nrow(distance) > 0){
    map <- map |>
      addAwesomeMarkers(data = distance,
                        lng = ~(st_coordinates(geometry_start)[,1] + st_coordinates(geometry_end)[,1])/2,
                        lat = ~(st_coordinates(geometry_start)[,2] + st_coordinates(geometry_end)[,2])/2,
                        label = "Segment anomaly (distance)",
                        icon = i_anomaly)}
  
  # -- starting point layer
  if(nrow(start) > 0){
    map <- map |>
      addAwesomeMarkers(data = start,
                        lng = ~(st_coordinates(geometry_start)[,1] + st_coordinates(geometry_end)[,1])/2,
                        lat = ~(st_coordinates(geometry_start)[,2] + st_coordinates(geometry_end)[,2])/2,
                        label = "Segment anomaly (Speed from starting point)",
                        icon = i_anomaly)}
  
  # -- return
  map
  
}
