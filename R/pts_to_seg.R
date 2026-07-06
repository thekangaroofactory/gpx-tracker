

#' Turn Points to Segments
#'
#' @param track the track table
#'
#' @returns the new track table
#' @export
#'
#' @examples
#' \notrun{
#' pts_to_seg(track)
#' }

pts_to_seg <- function(track){

  track |>
    mutate(segment_id = 1:length(point_id),
           datetime_end = lead(datetime),
           geometry_end = lead(geometry),
           elevation_end = lead(elevation)) |>
    rename(datetime_start = datetime,
           geometry_start = geometry,
           elevation_start = elevation) |>
    select(-(point_id)) |>
    relocate(segment_id) |>
    relocate(elevation_start, .after = geometry_start) |>
    filter(row_number() <= n() - 1)

}
