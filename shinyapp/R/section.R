


section <- function(segments, distances, type = c("slow", "fast")){

  # -- check arg
  type <- match.arg(type)
  
  # -- compute section bounds
  section_ids <- unlist(distances |> 
                          filter(time == ifelse(type == "slow", max(time), min(time))) |>
                          select(c("segment_id_start", "segment_id_end")))
  
  # -- return (section segments)
  segments |>
    slice(section_ids[[1]]:section_ids[[2]])
  
}
