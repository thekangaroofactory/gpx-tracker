

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
  track <- sf::read_sf(x, layer = "track_points", as_tibble = FALSE)
  
  # -- rework data
  track <- track |>
    select(c("track_seg_point_id", "ele", "time", "geometry")) |>
    mutate(lng = st_coordinates(geometry)[,1],
           lat = st_coordinates(geometry)[,2]) |>
    rename(point_id = track_seg_point_id,
           elevation = ele,
           datetime = time)
  
  # -- drop the sf object type
  # otherwise attribute sf_column makes it impossible to drop the column with dplyr
  track <- as.data.frame(track)
  attr(track, "sf_column") <- NULL
  attr(track, "agr") <- NULL
  
  # -- return
  # drop geometry once track is a normal data.frame
  track |> select(-geometry)
  
}
