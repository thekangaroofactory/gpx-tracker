

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
              tags$li(id = ns("toggle_1"), class = "toggle-active", titles[[1]], onclick = event),
              tags$li(id = ns("toggle_2"), titles[[2]], onclick = event),
              tags$li(id = ns("toggle_3"), "Slide 3", onclick = event)),
      
      # -- body (content)
      div(id = ns("slider"), class = "slider-body slider-active",
          div(id = ns("slide_1"),
              slides[[1]]),
          
          shinyjs::hidden(
          div(id = ns("slide_2"),
              slides[[2]])),
          
          shinyjs::hidden(
          div(id = ns("slide_3"),
              card(
                card_header("Slide 3"),
                p("Some content for the slide."))))
      ),
      
      # -- footer (dots)
      tags$ul(class = "toggles toggles-footer",
              tags$li(id = ns("toggle_dot_1"), class = "toggle-active", onclick = event),
              tags$li(id = ns("toggle_dot_2"), onclick = event),
              tags$li(id = ns("toggle_dot_3"), onclick = event)))
  
}
