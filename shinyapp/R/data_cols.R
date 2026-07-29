

#' Column Types
#' 
#' @description
#' Wrapper function to return data column types.
#'
#' @param type the name of the data
#'
#' @returns a column specification
#' @export
#'
#' @examples
#' data_cols("leg")

data_cols <- function(type){
  
  if(type == "leg")
    return(
      c(track_id = "character", segment_id = "integer", lng = "double", lat = "double",
        datetime = "POSIXct", elevation = "double", cum_distance = "double",
        type = "character", distance = "double"))
  
}
