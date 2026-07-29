

#' Compute Segment Stats
#' 
#' @description
#' Add basic time, distance, elevation gain stats to each segment.
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
  
  # -- compute distance (m)
  # convert lng/lat back to sf object
  df_start <- st_as_sf(track[c("lng_start", "lat_start")], coords = c("lng_start", "lat_start"), crs = 4326)
  df_end <- st_as_sf(track[c("lng_end", "lat_end")], coords = c("lng_end", "lat_end"), crs = 4326)
  distances <- st_distance(df_start, df_end, by_element = TRUE)
  units(distances) <- NULL
  track <- track |>
    mutate(distance = distances,
           cum_distance = cumsum(distance) / 1000)
  
  # -- add elevation gain (m)
  track <- track |>
    mutate(elevation_gain = elevation_end - elevation_start,
           slope = elevation_gain / distance * 100)

}
