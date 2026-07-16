

m_section <- function(map, section, type = c("slow", "fast")){
  
  # -- check arg
  type <- match.arg(type)
  
  # -- return (map)
  map |>
    
    # -- add section layer
    # need to manually add the latest end point since it's based on segments
    addPolylines(data = section,
                 lng = ~c(st_coordinates(geometry_start)[,1], st_coordinates(tail(geometry_end, 1))[,1]),
                 lat = ~c(st_coordinates(geometry_start)[,2], st_coordinates(tail(geometry_end, 1))[,2]), 
                 weight = 4, 
                 color = ifelse(type == "slow", "orange", "green")) |>
    
    fitBounds(lng1 = st_coordinates(head(section, n = 1L)$geometry_start)[1],
              lat1 = st_coordinates(head(section, n = 1L)$geometry_start)[2],
              lng2 = st_coordinates(tail(section, n = 1L)$geometry_end)[1],
              lat2 = st_coordinates(tail(section, n = 1L)$geometry_end)[2],
              options = list(padding = c(10, 10)))
}
