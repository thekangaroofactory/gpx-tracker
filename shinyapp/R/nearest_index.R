

#' Find Nearest Segment
#'
#' @param segments the segment table
#' @param lng a numeric value, the longitude
#' @param lat a numeric value, the latitude
#' 
#' @details
#' Computation is based on the segment end
#'
#' @returns an integer, index of the nearest segment (end)
#' @export
#'
#' @examples
#' \dontrun{
#' nearest_index(segments, 2.3522, 48.8566)
#' }

nearest_index <- function(segments, lng, lat){

  # -- convert lng lat into sf object
  ref_point <- st_sfc(st_point(c(lng, lat)), crs = 4326)
  df_end <- st_as_sf(segments[c("lng_end", "lat_end")], coords = c("lng_end", "lat_end"), crs = 4326)
  
  # -- return nearest index
  st_nearest_feature(ref_point, df_end)
  
}
