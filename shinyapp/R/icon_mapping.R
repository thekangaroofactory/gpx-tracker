

icon_mapping <- function(){
  
  # -- return mapping
  data.frame(type = c("start", "finish", "target", "leg", "click", "anomaly", "long", "overnight"),
             name = c("circle-play", "flag-checkered", "location-crosshairs", "flag", "ellipsis", "triangle-exclamation", "circle-pause", "campground"),
             markerColor = c("blue", "blue", "lightgray", "beige", "beige", "lightred", "lightgray", "lightgray"))
  
}
