

#' Leg Targets
#' 
#' @description
#' Compute the targets where to finish a leg
#' 
#' @param segments the segment table
#' @param start the id of the starting segment
#' @param min the minimum distance for the leg
#' @param max the maximum distance for the leg
#' @param step the step to compute sequence between min and max
#'
#' @returns a vector of segment ids
#' @export
#'
#' @examples
#' \dontrun{
#' leg_targets(segments)
#' }

leg_targets <- function(segments, start = 1, min = 70, max = 80, step = 5){

  # -- get cumulative distance at starting point
  ref_dist <- segments |> filter(segment_id == start) |> pull(cum_distance)
  
  # -- compute targets
  unlist(lapply(seq(from = min, to = max, by = step), function(x) which.min(abs(segments$cum_distance - ref_dist - x))))
  
}
