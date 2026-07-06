

#' Compute Elevation Stats
#'
#' @param track the track table with segment stats
#'
#' @returns a vector
#' @export
#'
#' @examples
#' \notrun{
#' elevation_stats(track)
#' }

elevation_stats <- function(track){

  # -- total positive gain
  pos_gain <- sum(track |>
        filter(elevation_gain > units::set_units(0, "m")) |>
        pull(elevation_gain))
  
  # -- total negative gain
  neg_gain <- sum(track |>
        filter(elevation_gain < units::set_units(0, "m")) |>
        pull(elevation_gain))

  # -- return
  c(min = min(track$elevation_start, track$elevation_end),
    max = max(track$elevation_start, track$elevation_end),
    positive_gain = pos_gain,
    negative_gain = neg_gain)
  
}
