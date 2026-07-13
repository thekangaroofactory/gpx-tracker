

layout_slider <- function(ui, namespace = NULL){
  
  # -- get namespace
  ns <- NS(namespace)
  event <- send_event("slider_event", id = namespace)
  
  # -- the slider component
  div(class = "slider",
      
      # -- header (toggles)
      tags$ul(class = "toggles toggles-header",
              tags$li(id = ns("toggle_1"), class = "toggle-active", "Slide 1", onclick = event),
              tags$li(id = ns("toggle_2"), "Slide 2", onclick = event),
              tags$li(id = ns("toggle_3"), "Slide 3", onclick = event)),
      
      # -- body (content)
      div(id = ns("slider"), class = "slider-body slider-active",
          div(id = ns("slide"),
              
              # -- initial content (slide)
              ui
              
          )),
      
      # -- footer (dots)
      tags$ul(class = "toggles toggles-footer",
              tags$li(id = ns("toggle_dot_1"), class = "toggle-active", onclick = event),
              tags$li(id = ns("toggle_dot_2"), onclick = event),
              tags$li(id = ns("toggle_dot_3"), onclick = event)))
  
}
