

#' Track Bounds
#' 
#' @description
#' + bound_start() returns track starting segment
#' + bound_finish() returns track finishing segment
#'
#' @param segments the segment table
#'
#' @returns a data.frame of the first or last row
#' @export
#'
#' @examples
#' \dontrun{
#' bound_start(segments)
#' bound_finish(segments)
#' }

bound_start <- function(segments){

  segments |> 
    head(n = 1L) |>
    select(segment_id, datetime_start, lng_start, lat_start, elevation_start) |>
    mutate(type = "start",
           distance = 0) |>
    rename(datetime = datetime_start,
           lng = lng_start,
           lat = lat_start,
           elevation = elevation_start)
  
}

bound_finish <- function(segments){
  
  segments |> 
    tail(n = 1L) |>
    select(segment_id, datetime_end, lng_end, lat_end, elevation_end, cum_distance) |>
    mutate(type = "finish") |>
    rename(datetime = datetime_end,
           lng = lng_end,
           lat = lat_end,
           elevation = elevation_end,
           distance = cum_distance)
  
}
