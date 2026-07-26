

bounding_box <- function(segments){
  
  # -- return
  c(
    lng1 = min(segments$lng),
    lat1 = min(segments$lat),
    lng2 = max(segments$lng),
    lat2 = max(segments$lat))
  
}
