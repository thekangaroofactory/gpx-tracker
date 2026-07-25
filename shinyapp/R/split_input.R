

#' Split Input Value
#'
#' @param value the value
#'
#' @returns a named vector c(id, action, value)
#' @export
#'
#' @examples
#' split_input("b13681f37a70c34dc83ff0a6abe9a4f0-add_leg_896")

split_input <- function(value){
  
  # -- split by '-'
  x <- unlist(strsplit(value, split = "-"))
  
  # -- split by '_'
  y <- unlist(strsplit(x[[2]], "_"))
  
  # -- return
  c(
    id = x[[1]],
    action = paste(y[-length(y)], collapse = "_"),
    value = gsub("^.*_", "", x[[2]]))
  
}
