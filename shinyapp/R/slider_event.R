

#' Slider Event
#'
#' @param input the shiny input object 
#' @param id the target input id
#'
#' @returns a string to pass to an onclick parameter
#' @export
#'
#' @examples
#' \notrun{
#' slider_event(input, id = "slider_event")
#' }

slider_event <- function(input, id){
  
  # -- get namespace
  ns <- NS(id)
  
  # -- return
  paste0('Shiny.setInputValue(\"', ns(input), '\", this.id, {priority: \"event\"})')
  
}
