

bounding_box <- function(segments){
  
  # -- return
  c(
    lng1 = min(st_coordinates(segments$geometry_start)[,1]),
    lat1 = min(st_coordinates(segments$geometry_start)[,2]),
    lng2 = max(st_coordinates(segments$geometry_end)[,1]),
    lat2 = max(st_coordinates(segments$geometry_end)[,2]))
  
}
