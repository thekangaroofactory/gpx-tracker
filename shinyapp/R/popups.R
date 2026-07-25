

#' Init Popup
#'
#' @param segments the segment table
#'
#' @returns a data.frame with popup column added (label)
#' @export
#'
#' @examples
#' \dontrun{
#' popup_label(track_bounds(segments))
#' }

popup_label <- function(segments){
  
  segments |>
    mutate(popup = paste(sep = "<br/>",
                         paste0("<b>", label, "</b>")))
  
}
 

#' Add Datetime
#'
#' @param segments a segment table with popup column
#'
#' @returns a segment table with updated popup
#' @export
#'
#' @examples
#' \dontrun{
#' popup_datetime(popup_label(track_bounds(segments)))
#' }

popup_datetime <- function(segments){
  
  segments |>
    mutate(popup = paste(sep = "<br/>",
                         popup,
                         format(case_when(label == "Start" ~ datetime_start, .default = datetime_end),'%Y-%m-%d, %H:%M:%S')))
  
}


popup_leg <- function(segments){          
           
  segments |>
    mutate(popup = case_when(label == "Start" ~paste(popup, "actionlink", sep = "<br/>"), .default = popup))
    
}
