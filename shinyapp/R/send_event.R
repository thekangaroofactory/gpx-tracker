

send_event <- function(input, id){
  
  ns <- NS(id)
  
  paste0('Shiny.setInputValue(\"', ns(input), '\", this.id, {priority: \"event\"})')
  
}
