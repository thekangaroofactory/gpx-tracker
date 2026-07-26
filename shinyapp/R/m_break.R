

#' Add Break(s)
#'
#' @param map a leaflet map object.
#' @param breaks the break summary table.
#'
#' @returns an updated leaflet map
#' @export
#'
#' @examples
#' \notrun{
#' m_break(map, breaks)
#' }

m_break <- function(map, breaks = NULL){
  
  # -- check parameter
  if(is.null(breaks) || !is.data.frame(breaks) || nrow(breaks) == 0)
    return(map)
  
  # -- declare icons
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
  
  # -- short
  map <- map %>% addCircleMarkers(data = breaks |> filter(type == "short"),
                                  lng = ~lng_end,
                                  lat = ~lat_end,
                                  radius = ~time/1000,
                                  popup = ~paste(sep = "<br/>",
                                                 "<b>Break (short)</b>",
                                                 paste0(floor(time / 60), "min")))
  
  # -- medium
  map <- map %>% addCircleMarkers(data = breaks |> filter(type == "medium"),
                                  lng = ~lng_end,
                                  lat = ~lat_end,
                                  color = "orange",
                                  radius = ~time/1000,
                                  popup = ~paste(sep = "<br/>",
                                                 "<b>Break (medium)</b>",
                                                 paste0(floor(time / 60), "min")))
  
  # -- long
  if("long" %in% breaks$type)
    map <- map %>% addAwesomeMarkers(data = breaks |> filter(type == "long"),
                                     lng = ~lng_end,
                                     lat = ~lat_end,
                                     icon = i_long,
                                     popup = ~paste(sep = "<br/>",
                                                    "<b>Break (long)</b>",
                                                    paste0(floor(time / 60), "min")))
  
  # -- overnight
  if("overnight" %in% breaks$type)
    map <- map %>% addAwesomeMarkers(data = breaks |> filter(type == "overnight"),
                                     lng = ~lng_end,
                                     lat = ~lat_end,
                                     icon = i_overnight,
                                     popup = ~paste(sep = "<br/>",
                                                    "<b>Break (overnight)</b>",
                                                    paste0(floor(time / 3600), "h", floor((time - floor(time / 3600) * 3600) / 60), "min")))
  
  # -- return
  map
  
}
