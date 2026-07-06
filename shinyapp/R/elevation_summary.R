

#' Compute Elevation Stats
#'
#' @param track the track table with segment stats
#'
#' @returns a vector
#' @export
#'
#' @examples
#' \notrun{
#' elevation_summary(track)
#' }

elevation_summary <- function(track){

  # -- total positive gain
  pos_gain <- sum(track |>
        filter(elevation_gain > 0) |>
        pull(elevation_gain))
  
  # -- total negative gain
  neg_gain <- sum(track |>
        filter(elevation_gain < 0) |>
        pull(elevation_gain))

  # -- return
  c(lowest = min(track$elevation_start, track$elevation_end),
    highest = max(track$elevation_start, track$elevation_end),
    pos_gain = pos_gain,
    neg_gain = neg_gain)
  
}
