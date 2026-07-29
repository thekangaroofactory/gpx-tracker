

#' Action Link Input(s)
#' 
#' @description
#' Wrapper function around shiny::actionLink() to produce multiple action
#' links with unique inputId's and same target input.
#' 
#' @param id a vector or list of ids.
#' @param label the label for the actionLink.
#' @param target the name of the target input.
#' @param ns the namespace function to use.
#' @param pattern the pattern to generate unique inputId's.
#' @param as_character a logical if the shiny.tag should be returned or a character value.
#'
#' @details
#' The actionLink inputId will be computed based on `ns("pattern_id")` so that it should be unique
#' across different module instances.
#' 
#' All produced actionLinks will target the same input with different values.
#' `input$target` will receive the inputId, from which one can extract the source id for example.
#'
#' If used from the main server, skip `ns`.
#'
#' @returns a list of shiny tags or character values.
#' @export
#'
#' @examples

action_link <- function(id, label, target, ns = function(x) x, pattern = "action_link", as_character = FALSE){
  
  lapply(id, function(x){
    
    a <- actionLink(inputId = ns(paste0(pattern, "_", x)),
                    label = label,
                    onclick = paste0('Shiny.setInputValue(\"', ns(target), '\", this.id, {priority: \"event\"})'))
    
    if(as_character)
      paste(a)
    else a
    
  })
  
}
