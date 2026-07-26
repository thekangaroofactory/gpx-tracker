

icon_mapping <- function(){
  
  # -- return mapping
  data.frame(type = c("start", "finish", "target", "leg", "click"),
             name = c("circle-play", "flag-checkered", "location-crosshairs", "flag", "ellipsis"),
             markerColor = c("blue", "blue", "lightgray", "beige", "beige"))
  
}
