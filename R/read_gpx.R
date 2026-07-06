

#' Read GPX file
#'
#' @param x the file to read
#'
#' @returns a sf object
#' @export
#'
#' @examples
#' \notrun{
#' read_gpx("./foo.gpx")
#' }

read_gpx <- function(x){

  # -- load track
  track <- sf::read_sf(x, layer = "track_points")
  
  # -- filter unused columns & rename
  track |>
    select(c("track_seg_point_id", "ele", "time", "geometry")) |>
    rename(point_id = track_seg_point_id,
           elevation = ele,
           datetime = time)
  
}
