

layout_slider <- function(..., namespace = NULL){
  
  # -- get namespace
  ns <- NS(namespace)
  event <- send_event("slider_event", id = namespace)
  
  # -- get slides
  slides <- list(...)
  nb_slides <- length(slides)
  titles <- names(slides)
  
  # -- the slider component
  div(class = "slider",
      
      # -- header (toggles)
      tags$ul(class = "toggles toggles-header",
  
              lapply(1:nb_slides, function(x) tags$li(id = ns(paste0("toggle_", x)), 
                                                           class = if(x == 1) "toggle-active", 
                                                           titles[[x]], 
                                                           onclick = event))),
      
      # -- body (content)
      div(id = ns("slider"), class = "slider-body slider-active",
          
          lapply(1:nb_slides, function(x)
            if(x == 1)
              div(id = ns(paste0("slide_", x)), slides[[x]])
            else
              shinyjs::hidden(div(id = ns(paste0("slide_", x)), slides[[x]])))),
      
      # -- footer (dots)
      tags$ul(class = "toggles toggles-footer",
              
              lapply(1:length(titles), function(x) tags$li(id = ns(paste0("toggle_dot_", x)), 
                                                           class = if(x == 1) "toggle-active",
                                                           onclick = event)))
  
  ) # slider
  
}
