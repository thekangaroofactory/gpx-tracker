

layout_slider_listener <- function(namespace, input, ...){
  
  # -- get namespace
  ns <- NS(namespace)
  
  # -- list of slides
  slides <- list(...)
  
  # -- slider listener
  # event on toggles & dots
  observeEvent(input$slider_event, {
    
    # -- extract target slide nb
    target_slide <- gsub(".*?-", "", input$slider_event) |> 
      gsub(pattern = "toggle_", replacement = "") |> 
      gsub(pattern = "dot_", replacement = "")
    cat("Slider display slide =", target_slide, "\n")

    # -- new content
    slide <- slides[[as.numeric(target_slide)]]

    # -- close slide
    shinyjs::toggleClass(selector = '.slider-active', class = 'slider-active')
    shinyjs::toggleClass(selector = '.toggle-active', class = 'toggle-active')
    
    # -- wait for css transition
    Sys.sleep(0.75)
    
    # -- replace body / slide content
    removeUI(selector = paste0('#', ns('slide')), immediate = T)
    insertUI(selector = paste0('#', ns('slider')), where = "afterBegin", ui = div(id = ns("slide"), slide))
    
    # -- open slide
    shinyjs::toggleClass(selector = paste0('#', ns('slider')), class = 'slider-active')
    shinyjs::toggleClass(selector = paste0('#', ns(paste0('toggle_', target_slide))), class = 'toggle-active')
    shinyjs::toggleClass(selector = paste0('#', ns(paste0('toggle_dot_', target_slide))), class = 'toggle-active')
    
  })
  
}
