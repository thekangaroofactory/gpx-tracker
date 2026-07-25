

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


#' Add Leg
#'
#' @param segments a segment table with popup column
#'
#' @returns a segment table with updated popup
#' @export
#'
#' @examples
#' \dontrun{
#' popup_leg(popup_label(track_bounds(segments)))
#' }

popup_leg <- function(segments, ns){          
  
  helper <- function(x)
    lapply(x, function(x)
      paste(actionLink(inputId = ns(paste0("init_leg_", x)),
                 label = "Add leg",
                 onclick = paste0('Shiny.setInputValue(\"', ns("init_leg"), '\", this.id, {priority: \"event\"})'))))
    
  segments |>
    mutate(popup = replace_when(popup, label == "Start" ~paste(popup, helper(segment_id), sep = "<br/>")))
  
}
