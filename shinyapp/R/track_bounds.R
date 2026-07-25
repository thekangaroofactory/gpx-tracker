

#' Track Bounds
#'
#' @param segments the segment table
#'
#' @returns a data.frame of the first and last rows
#' @export
#'
#' @examples
#' \dontrun{
#' track_bounds(segments)
#' }

track_bounds <- function(segments){

  segments |> 
    filter(row_number() %in% c(1, n())) |>
    mutate(label = c("Start", "Finish"))
  
}
