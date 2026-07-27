
# ------------------------------------------------------------------------------
# dedicated marker popup functions
# ------------------------------------------------------------------------------

popup_start <- function(markers, ns, plan = FALSE){
  
  x <- popup_title(markers$type)
  x <- if(plan)
    x |> popup_append(popup_show_targets(markers$segment_id, ns), suffix = "leg targets.")
  else
    x |> popup_append(popup_datetime(markers$datetime))
  markers |> popup(x)
  
}

popup_finish <- function(markers, plan = FALSE){

  x <- popup_title(markers$type)
  if(!plan)
    x <- x |> popup_append(popup_datetime(markers$datetime))
  x <- x |> popup_append(popup_distance(markers$cum_distance))
  markers |> popup(x)
  
}

popup_target <- function(markers, ns){
  
  x <- popup_title(markers$type) |>
    popup_append(popup_clear_group(markers$segment_id, ns), suffix = "targets.")
  markers |> popup(x)
  
}

popup_leg_click <- function(markers, ns){
  
  x <- popup_title(markers$type) |>
    popup_append(popup_create_leg(markers$segment_id, ns), suffix = "leg here.")
  markers |> popup(x)
  
}

popup_leg <- function(markers, ns){
  
  x <- popup_title(markers$type) |>
    popup_append(popup_distance(markers$cum_distance), prefix = "Distance from start:") |>
    popup_append(popup_distance(markers$distance), prefix = "Leg distance:") |>
    popup_append(popup_show_targets(markers$segment_id, ns), suffix = "leg targets.") |>
    popup_append(popup_remove_leg(markers$segment_id, ns), suffix = "leg.")
  markers |> popup(x)
  
}

popup_anomaly_speed <- function(markers){
  
  x <- popup_title(markers$type) |>
    popup_append(popup_speed(markers$speed))
  markers |> popup(x)
  
}

popup_anomaly_distance <- function(markers){
  
  x <- popup_title(markers$type) |>
    popup_append(popup_distance(markers$distance, unit = "m"))
  markers |> popup(x)
  
}

popup_break <- function(markers){
  
  x <- popup_title(markers$type) |>
    popup_append(popup_time(markers$time))
  markers |> popup(x)
  
}


# ------------------------------------------------------------------------------
# base functions
# ------------------------------------------------------------------------------

popup <- function(markers, x){
  markers |> mutate(popup = x)}

popup_append <- function(popup, x, prefix = NULL, suffix = NULL){
  paste(sep = "<br/>", popup, gsub("^\\s+|\\s+$", "", paste(prefix, x, suffix)))}

popup_title <- function(x){
  ktools::toupper_words(x)}

popup_datetime <- function(x){
  format(x, '%Y-%m-%d, %H:%M:%S')}

popup_time <- function(x){
  ifelse(x < 3600, paste0(floor(x / 60), "min"), paste0(floor(x / 3600), "h", floor((x - floor(x / 3600) * 3600) / 60), "min"))}

popup_distance <- function(x, digits = 0, unit = "km"){
  paste0(round(x, digits = digits), unit)}

popup_speed <- function(x){
  paste0(round(x, digits = 1), "km/h")}


# ------------------------------------------------------------------------------
# action link functions
# ------------------------------------------------------------------------------

popup_show_targets <- function(x, ns){
  
  action_link(id = x, 
              label = "Show", 
              target = "leg_targets", 
              ns = ns, 
              pattern = "show_targets", 
              as_character = TRUE)
  
}
  
popup_clear_group <- function(x, ns){
  
  action_link(id = x, 
              label = "Clear", 
              target = "clear_group", 
              ns = ns, 
              pattern = "leg_target", 
              as_character = TRUE)
  
}
  
popup_create_leg <- function(x, ns){
  
  action_link(id = x, 
              label = "Add", 
              target = "create_leg", 
              ns = ns, 
              pattern = "add_leg", 
              as_character = TRUE)
  
}

popup_remove_leg <- function(x, ns){
  
  action_link(id = x, 
              label = "Remove", 
              target = "drop_leg", 
              ns = ns, 
              pattern = "remove_leg", 
              as_character = TRUE)
  
}
