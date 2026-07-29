

#' flyToBounds Wrapper
#'
#' @param map a leaflet map object
#' @param bounds a vector of bounds (most probably the output of `bounding_box()`)
#'
#' @export
#'
#' @examples

m_fly_bounds <- function(map, bounds){
  
  map |>
    flyToBounds(lng1 = bounds[['lng1']],
                lat1 = bounds[['lat1']],
                lng2 = bounds[['lng2']],
                lat2 = bounds[['lat2']])
  
}
