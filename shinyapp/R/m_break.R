

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
  
  # -- short
  map <- map %>% addCircleMarkers(data = breaks |> filter(type == "short"),
                                  lng = ~lng,
                                  lat = ~lat,
                                  radius = ~time/1000,
                                  popup = ~paste(sep = "<br/>",
                                                 "<b>Break (short)</b>",
                                                 paste0(floor(time / 60), "min")))
  
  # -- medium
  map <- map %>% addCircleMarkers(data = breaks |> filter(type == "medium"),
                                  lng = ~lng,
                                  lat = ~lat,
                                  color = "orange",
                                  radius = ~time/1000,
                                  popup = ~paste(sep = "<br/>",
                                                 "<b>Break (medium)</b>",
                                                 paste0(floor(time / 60), "min")))
  
  # -- return
  map
  
}
