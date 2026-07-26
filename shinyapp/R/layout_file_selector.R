

#' File Selector Layout
#'
#' @param files a vector of the filenames
#'
#' @returns an HTML tag
#' @export
#'
#' @examples
#' \dontrun{
#' layout_file_selector(c("foo.gpx", "bar.gpx"))
#' }

layout_file_selector <- function(files){
  
  # -- return
  layout_column_wrap(
    width = "250px",
    fixed_width = TRUE,
    
    !!!lapply(files, function(x) {
      
      # -- return
      card(
        card_header("itinerary"),
        track_title(file.path(Sys.getenv("DATA_HOME"), x)),
        card_footer(actionLink(inputId = x, 
                               label = "open",
                               onclick = 'Shiny.setInputValue(\"open_track\", this.id, {priority: \"event\"})')))}))
  
}
