

#' Build Track Map
#'
#' @param data the track segment table.
#' @param idle an integer to tune the break threshold.
#'
#' @returns a leaflet map
#' @export
#'
#' @examples
#' \notrun{
#' m_track(data)
#' }

m_track <- function(data, idle = 60){

  # -- return
  leaflet(data) %>%
    
    # -- the map
    addTiles() %>%
    
    # -- add track layer
    # need to manually add the latest end point since it's based on segments
    addPolylines(lng = ~c(st_coordinates(geometry_start)[,1], st_coordinates(tail(geometry_end, 1))[,1]),
                 lat = ~c(st_coordinates(geometry_start)[,2], st_coordinates(tail(geometry_end, 1))[,2]), 
                 weight = 2, 
                 color = "black") %>%
    
    # -- add breaks layer
    # basically segments with time > threshold
    addCircleMarkers(data = data |> filter(time > idle),
                     lng = ~st_coordinates(geometry_start)[,1],
                     lat = ~st_coordinates(geometry_start)[,2],
                     radius = ~time/1000)
  
}
