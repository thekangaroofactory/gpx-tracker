

#' Build Track Map
#'
#' @param data the track segment table.
#' @param breaks the break summary table.
#'
#' @returns a leaflet map
#' @export
#'
#' @examples
#' \notrun{
#' m_track(data)
#' }

m_track <- function(data, breaks = NULL){

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
    
    # -- add breaks layer
    if(is.data.frame(breaks))
      
      m <- m %>% addCircleMarkers(data = breaks,
                                  lng = ~st_coordinates(geometry_start)[,1],
                                  lat = ~st_coordinates(geometry_start)[,2],
                                  radius = ~time/3600)
    
}
