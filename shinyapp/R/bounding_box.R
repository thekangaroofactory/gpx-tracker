

bounding_box <- function(segments){
  
  # -- return
  c(
    lng1 = min(st_coordinates(segments$geometry)[,1]),
    lat1 = min(st_coordinates(segments$geometry)[,2]),
    lng2 = max(st_coordinates(segments$geometry)[,1]),
    lat2 = max(st_coordinates(segments$geometry)[,2]))
  
}
