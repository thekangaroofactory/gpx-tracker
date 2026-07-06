

#' Compute Segment Stats
#' 
#' @description
#' Add basic time, distance, speed, elevation gain stats to each segment.
#'
#' @param track the track table
#'
#' @returns a track table with segment stats
#' @export
#'
#' @examples
#' \notrun(
#' seg_stats(track)
#' )

seg_stats <- function(track){
  
  # -- compute time (secs)
  track <- track |>
    mutate(time = as.numeric(difftime(datetime_end, datetime_start, units = "secs")))
  units(track$time) <- "secs"
  
  # -- compute distance (m)
  track <- track |>
    mutate(distance = st_distance(geometry_start, geometry_end, by_element = T))
  
  # -- compute speed (km/h)
  track <- track |>
    mutate(speed = distance / time)
  units(track$speed) <- "km/h"
  
  # -- add elevation gain (m)
  track <- track |>
    mutate(elevation_gain = elevation_end - elevation_start)

}
