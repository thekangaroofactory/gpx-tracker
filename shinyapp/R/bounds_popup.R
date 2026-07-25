

#' Add Bounds Popup
#'
#' @param bounds the bounds segment table
#'
#' @returns a data.frame with popup column added
#' @export
#'
#' @examples
#' \dontrun{
#' bounds_popup(track_bounds(segments))
#' }

bounds_popup <- function(bounds){
  
  bounds |>
    
    mutate(popup = paste(sep = "<br/>",
                   paste0("<b>", label, "</b>"),
                   format(case_when(label == "Start" ~ datetime_start, .default = datetime_end),'%Y-%m-%d, %H:%M:%S')))
  
}
