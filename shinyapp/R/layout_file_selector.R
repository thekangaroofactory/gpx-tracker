

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
    !!!lapply(files, function(x) {
      
      # -- return
      card(
        card_header("Itinary"),
        x,
        card_footer(actionLink(inputId = x, 
                               label = "open",
                               onclick = 'Shiny.setInputValue(\"open_track\", this.id, {priority: \"event\"})')))}))
  
}
