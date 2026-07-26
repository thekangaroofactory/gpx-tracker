

#' Track Anomalies
#'
#' @param segments the segment table
#' @param max_speed a numeric value to consider as a speed limit
#' @param max_distance a numeric value to consider as a distance limit (m)
#'
#' @returns an anomaly table (points)
#' @export
#'
#' @examples

track_anomalies <- function(segments, max_speed = 100, max_distance = 1000){
  
  # -- speed
  fu_speed <- segments |> 
    filter(speed > max_speed) |> 
    mutate(type = "anomaly",
           label = "Segment anomaly (speed)",
           lng = (lng_start + lng_end)/2,
           lat = (lat_start + lat_end)/2) |>
    mk_popup(info = c("title", "speed"))
           
  if(nrow(fu_speed) > 0)
    warning("Speed anomaly detected: ", paste(fu_speed$speed, collapse = " / "), call. = F)
  
  # -- distance
  fu_distance <- segments |> 
    filter(distance > max_distance) |> 
    mutate(type = "anomaly",
           label = "Segment anomaly (distance)",
           lng = (lng_start + lng_end)/2,
           lat = (lat_start + lat_end)/2) |>
    mk_popup(info = c("title", "distance_m"))
           
  if(nrow(fu_distance) > 0)
    warning("Distance anomaly detected: ", paste(fu_distance$distance, collapse = " / "), call. = F)
  
  # -- starting speed
  fu_speed_start <- segments |> 
    filter(segment_id == 1 & speed > 20) |> 
    mutate(type = "anomaly",
           label = "Segment anomaly (Speed from starting point)",
           lng = (lng_start + lng_end)/2,
           lat = (lat_start + lat_end)/2) |>
    mk_popup(info = c("title", "speed"))
           
  if(nrow(fu_speed_start) > 0)
    warning("Speed / start anomaly detected: ", fu_speed_start$speed, call. = F)
  
  # -- return
  bind_rows(fu_speed, fu_distance, fu_speed_start)
  
}
