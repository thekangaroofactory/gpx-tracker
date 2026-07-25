

#' Add Track Bounds
#'
#' @param map the map object to update
#'
#' @returns a map object
#' @export
#'
#' @examples

m_start_finish <- function(map, data){
  
  # -- icons
  i_start <- makeAwesomeIcon(
    icon = "circle-play",
    library = "fa")
  
  i_finish <- makeAwesomeIcon(
    icon = "flag-checkered",
    library = "fa")
  
  # -- return
  map |>
    
    # -- add starting point
    addAwesomeMarkers(data = head(data, n = 1L),
                      lng = ~st_coordinates(geometry_start)[,1],
                      lat = ~st_coordinates(geometry_start)[,2],
                      icon = i_start,
                      popup = ~paste(sep = "<br/>",
                                     "<b>Start</b>",
                                     format(datetime_start,'%Y-%m-%d, %H:%M:%S'))) |>
    
    # -- add finish point
    addAwesomeMarkers(data = tail(data, n = 1L),
                      lng = ~st_coordinates(geometry_end)[,1],
                      lat = ~st_coordinates(geometry_end)[,2],
                      icon = i_finish,
                      popup = ~paste(sep = "<br/>",
                                     "<b>Finish</b>",
                                     format(datetime_start,'%Y-%m-%d, %H:%M:%S')))
  
}
