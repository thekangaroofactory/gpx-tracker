

#' Add Markers
#'
#' @param map the leaflet map object to update
#' @param data the marker table
#' 
#' @details
#' The marker table is expected to contain the following columns:
#' - type (the marker type)
#' - geometry (the coordinates where to put the markers)
#' - popup (the content of the popup)
#'
#' @returns an updated leaflet map object
#' @export
#'
#' @examples

m_marker <- function(map, markers, layerId = NULL, group = NULL){
  
  # -- get icon mapping
  i_map <- icon_mapping()
  
  # -- check label
  if(!"label" %in% names(markers))
    markers$label <- ktools::toupperfirst(markers$type)
  
  # -- update map
  map |>
    
    # -- add starting point
    addAwesomeMarkers(data = markers,
                      lng = ~st_coordinates(geometry)[,1],
                      lat = ~st_coordinates(geometry)[,2],
                      layerId = layerId,
                      group = group,
                      icon = ~makeAwesomeIcon(icon = i_map[match(markers$type, i_map$type), 'name'], 
                                              library = "fa", 
                                              markerColor = i_map[match(markers$type, i_map$type), 'markerColor']),
                      popup = ~popup,
                      label = ~label)
  
}
