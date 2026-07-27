

#' Read Track Data
#' 
#' @description
#' Wrapper function around iker::read_data()
#' 
#' @param type the type of data to read
#'
#' @returns a data.frame of the track data
#' @export
#'
#' @examples

read_track_data <- function(type = c("leg")){
  
  # -- read
  x <- iker::read_data(file = "legs.csv", col_types = data_cols(type))
  
  # -- return
  if(nrow(x) == 0) 
    NULL
  else
    as.data.frame(x)
  
}

