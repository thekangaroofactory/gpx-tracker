

#' Build Track Map
#' 
#' @description
#' This is meant to be the baseline function to build the map with the track.
#' Other m_* functions will update the baseline map.
#'
#' @param data the track segment table.
#'
#' @returns a leaflet map
#' @export
#'
#' @examples
#' \notrun{
#' m_track(data)
#' }

m_track <- function(data){
 
  # -- return
  m <- leaflet(data) %>%
    
    # -- the map
    addTiles() %>%
    
    # -- add track layer
    # need to manually add the latest end point since it's based on segments
    addPolylines(lng = ~c(st_coordinates(geometry_start)[,1], st_coordinates(tail(geometry_end, 1))[,1]),
                 lat = ~c(st_coordinates(geometry_start)[,2], st_coordinates(tail(geometry_end, 1))[,2]), 
                 weight = 2, 
                 color = "black")
    
  # -- return
  m
  
}
