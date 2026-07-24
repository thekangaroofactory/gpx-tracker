

#' Compute Speed Stats
#'
#' @param track the track table
#'
#' @returns a track table with speed stats
#' @export
#'
#' @examples
#' \notrun(
#' speed_stats(track)
#' )

speed_stats <- function(track){
  
  # -- compute speed (km/h)
  track <- track |>
    mutate(speed = distance / time * 3.6,
           cummean_speed = cummean(speed))
  
}
