

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
                        lng = ~(lng_start + lng_end)/2,
                        lat = ~(lat_start + lat_end)/2,
                        label = "Segment anomaly (speed)",
                        icon = i_anomaly)}
  
  # -- distance layer
  if(nrow(distance) > 0){
    map <- map |>
      addAwesomeMarkers(data = distance,
                        lng = ~(lng_start + lng_end)/2,
                        lat = ~(lat_start + lat_end)/2,
                        label = "Segment anomaly (distance)",
                        icon = i_anomaly)}
  
  # -- starting point layer
  if(nrow(start) > 0){
    map <- map |>
      addAwesomeMarkers(data = start,
                        lng = ~(lng_start + lng_end)/2,
                        lat = ~(lat_start + lat_end)/2,
                        label = "Segment anomaly (Speed from starting point)",
                        icon = i_anomaly)}
  
  # -- return
  map
  
}
