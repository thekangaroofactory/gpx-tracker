

layout_slider_listener <- function(namespace, input){
  
  # -- get namespace
  ns <- NS(namespace)
  
  # -- slider listener
  # event on toggles & dots
  observeEvent(input$slider_event, {
    
    # -- extract target slide nb
    target_slide <- gsub(".*?-", "", input$slider_event) |> 
      gsub(pattern = "toggle_", replacement = "") |> 
      gsub(pattern = "dot_", replacement = "")
    cat("Display slide =", target_slide, "\n")

    # -- close slide
    shinyjs::toggleClass(selector = '.slider-active', class = 'slider-active')
    shinyjs::toggleClass(selector = '.toggle-active', class = 'toggle-active')
    
    # -- wait for css transition
    Sys.sleep(0.5)
    
    # -- set active slide
    shinyjs::hide(selector = paste0('[id^=', ns('slide_'), ']'))
    shinyjs::show(selector = paste0('#', ns('slide_'), target_slide))
    
    # -- open slide
    shinyjs::toggleClass(selector = paste0('#', ns('slider')), class = 'slider-active')
    shinyjs::toggleClass(selector = paste0('#', ns(paste0('toggle_', target_slide))), class = 'toggle-active')
    shinyjs::toggleClass(selector = paste0('#', ns(paste0('toggle_dot_', target_slide))), class = 'toggle-active')
    
  })
  
}
