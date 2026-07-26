

m_section <- function(map, section, type = c("slow", "fast")){
  
  # -- check arg
  type <- match.arg(type)
  
  # -- return (map)
  map |>
    
    # -- add section layer
    # need to manually add the latest end point since it's based on segments
    addPolylines(data = section,
                 lng = ~c(lng_start, tail(lng_end, 1)),
                 lat = ~c(lat_start, tail(lat_end, 1)), 
                 weight = 4, 
                 color = ifelse(type == "slow", "orange", "green")) |>
    
    fitBounds(lng1 = head(section$lng_start, n = 1L),
              lat1 = head(section$lat_start, n = 1L),
              lng2 = tail(section$lng_end, n = 1L),
              lat2 = tail(section$lat_end, n = 1L),
              options = list(padding = c(10, 10)))
}
