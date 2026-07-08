

#' Track Timeline
#'
#' @param steps the milestones table
#'
#' @returns a list of html tags
#' @export
#'
#' @examples
#' \notrun{
#' timeline(steps)
#' }

timeline <- function(steps){
  
  icon_list <- list(start = "circle-play",
                    long = "circle-stop",
                    overnight = "campground",
                    finish = "flag-checkered")
  
  # -- return
  lapply(1:nrow(steps), function(x){

    x <- steps[x, ]
    
    card(
      style = "background-color:#f4f3ef;",
      card_body(
        gap = 0,
        div(icon(icon_list[[x$type]]), tags$b(x$type)),
        format(x$datetime_start, "%Y-%m-%d %H:%M"), br(),
        x$city))
    
  })
    
}
