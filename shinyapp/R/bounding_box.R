

bounding_box <- function(points = NULL, segments = NULL){
  
  # -- return
  if(!is.null(points))
    
    c(
      lng1 = min(points$lng),
      lat1 = min(points$lat),
      lng2 = max(points$lng),
      lat2 = max(points$lat))
  
  else
    
    c(
      lng1 = min(segments$lng_start, segments$lng_end),
      lat1 = min(segments$lat_start, segments$lat_end),
      lng2 = max(segments$lng_start, segments$lng_end),
      lat2 = max(segments$lat_start, segments$lat_end))
    
}
