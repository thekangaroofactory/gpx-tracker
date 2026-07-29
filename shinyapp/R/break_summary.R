

#' Find Breaks
#'
#' @param data the segment table
#' @param idle floor time (s) to consider a segment as break (default = 60)
#' @param short ceiling time (m) to consider a break as short
#' @param long floor time (m) to consider a break as long
#'
#' @returns a break point table
#' @export
#'
#' @examples

break_summary <- function(data, idle = 60, short = 10, long = 30){

  # -- turn params min into sec
  short <- short * 60
  long <- long * 60
  
  # -- compute breaks & type
  data |> 
    filter(time > idle) |>
    mutate(type = case_when(as.Date(datetime_start) != as.Date(datetime_end) & time > 4 * 3600 ~ "overnight",
                            time >= long ~ "long",
                            time > short & time < long ~ "medium",
                            .default = "short")) |>
    rename(lng = lng_end,
           lat = lat_end,
           datetime = datetime_start,
           elevation = elevation_start) |>
    select(segment_id, lng, lat, datetime, elevation, time, cum_distance, type)

}
