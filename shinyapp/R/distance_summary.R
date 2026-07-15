

#' Distance Summary
#'
#' @param data the track segment table.
#' @param n an integer (default = 4) to tune the number of intervals.
#'
#' @returns a distance milestone table
#' @export
#'
#' @examples
#' \notrun{
#' distance_summary(data, n = 10)
#' }

distance_summary <- function(data, n = 4, dist = NULL, overnight = NULL){
  
  # -- compute interval
  if(is.null(dist))
    dist <- max(data$cum_distance) / n
  else  
    n <- max(data$cum_distance) / dist
  
  # -- get list of index
  idx <- unlist(lapply(1:(n - 1) * dist, function(x) which.min(abs(data$cum_distance - x))))
  
  # -- build summary
  df <- as.data.frame(data) |> 
    slice(sort(c(1, idx, nrow(data)))) |>
    select(c("segment_id", "datetime_start", "datetime_end", "cum_distance")) |>
    
    mutate(segment_id_end = lead(segment_id),
           datetime_end = lead(datetime_start),
           time = datetime_end - datetime_start,
           cum_distance = lead(cum_distance)) |>
    
    rename(segment_id_start = segment_id) |>
    slice(-n()) |>
    mutate(section_id = row_number(), .before = 1)
  
  # -- remove overnight break
  if(is.data.frame(overnight) && nrow(overnight) > 0){
    df <- df |> 
      mutate(time = if_else(datetime_start <= overnight$datetime_start & datetime_end >= overnight$datetime_end, 
                            time - (overnight$datetime_end - overnight$datetime_start),
                            time))}
  
  # -- return
  df
  
}
