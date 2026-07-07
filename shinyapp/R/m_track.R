

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
  
  # -- declare icons
  i_start <- makeAwesomeIcon(
    icon = "circle-play",
    library = "fa")
  
  i_finish <- makeAwesomeIcon(
    icon = "flag-checkered",
    library = "fa")
  
  i_medium <- makeAwesomeIcon(
    icon = "circle-pause",
    library = "fa",
    markerColor = "lightgray")
  
  i_long <- makeAwesomeIcon(
    icon = "circle-stop",
    library = "fa",
    markerColor = "lightgray")
  
  i_overnight <- makeAwesomeIcon(
    icon = "campground",
    library = "fa",
    markerColor = "lightgray")
  
  # -- return
  m <- leaflet(data) %>%
    
    # -- the map
    addTiles() %>%
    
    # -- add track layer
    # need to manually add the latest end point since it's based on segments
    addPolylines(lng = ~c(st_coordinates(geometry_start)[,1], st_coordinates(tail(geometry_end, 1))[,1]),
                 lat = ~c(st_coordinates(geometry_start)[,2], st_coordinates(tail(geometry_end, 1))[,2]), 
                 weight = 2, 
                 color = "black") %>%
    
    # -- add starting point
    addAwesomeMarkers(data = head(data, n = 1L),
                      lng = ~st_coordinates(geometry_start)[,1],
                      lat = ~st_coordinates(geometry_start)[,2],
                      icon = i_start,
                      popup = ~paste(sep = "<br/>",
                                     "<b>Start</b>",
                                     format(datetime_start,'%Y-%m-%d, %H:%M:%S'))) %>%
    
    # -- add finish point
    addAwesomeMarkers(data = tail(data, n = 1L),
                      lng = ~st_coordinates(geometry_end)[,1],
                      lat = ~st_coordinates(geometry_end)[,2],
                      icon = i_finish,
                      popup = ~paste(sep = "<br/>",
                                     "<b>Finish</b>",
                                     format(datetime_start,'%Y-%m-%d, %H:%M:%S')))
  
  
  # -- add breaks layer
  if(is.data.frame(breaks)){
    
    # -- short
    m <- m %>% addCircleMarkers(data = breaks |> filter(type == "short"),
                                lng = ~st_coordinates(geometry_end)[,1],
                                lat = ~st_coordinates(geometry_end)[,2],
                                radius = ~time/1000,
                                popup = ~paste(sep = "<br/>",
                                               "<b>Break (short)</b>",
                                               paste0(floor(time / 60), "min")))
    
    # -- medium
    m <- m %>% addCircleMarkers(data = breaks |> filter(type == "medium"),
                                lng = ~st_coordinates(geometry_end)[,1],
                                lat = ~st_coordinates(geometry_end)[,2],
                                color = "orange",
                                radius = ~time/1000,
                                popup = ~paste(sep = "<br/>",
                                               "<b>Break (medium)</b>",
                                               paste0(floor(time / 60), "min")))
    
    # -- long
    if("long" %in% breaks$type)
      m <- m %>% addAwesomeMarkers(data = breaks |> filter(type == "long"),
                                   lng = ~st_coordinates(geometry_end)[,1],
                                   lat = ~st_coordinates(geometry_end)[,2],
                                   icon = i_long,
                                   popup = ~paste(sep = "<br/>",
                                                  "<b>Break (long)</b>",
                                                  paste0(floor(time / 60), "min")))
    
    # -- overnight
    if("overnight" %in% breaks$type)
      m <- m %>% addAwesomeMarkers(data = breaks |> filter(type == "overnight"),
                                   lng = ~st_coordinates(geometry_end)[,1],
                                   lat = ~st_coordinates(geometry_end)[,2],
                                   icon = i_overnight,
                                   popup = ~paste(sep = "<br/>",
                                                  "<b>Break (overnight)</b>",
                                                  paste0(floor(time / 3600), "h", floor((time - floor(time / 3600) * 3600) / 60), "min")))
    
  }
  
  # -- return
  m
  
}
