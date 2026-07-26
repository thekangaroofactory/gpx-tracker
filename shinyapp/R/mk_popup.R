

mk_popup <- function(markers, info = c("title", "datetime", "show_targets", "clear_targets", "add_leg", "remove_leg", "distance", "distance_m", "cum_distance", "speed"), ns = NULL){
  
  # -- build popup
  popup <- paste0("<b>", ktools::toupperfirst(markers$type), "</b>")
  
  # ----------------------------------------------------------------------------
  # datetime
  # ----------------------------------------------------------------------------
  
  if("datetime" %in% info)
    popup <- paste(sep = "<br/>", popup, format(markers$datetime, '%Y-%m-%d, %H:%M:%S'))
  
  
  # ----------------------------------------------------------------------------
  # distance
  # ----------------------------------------------------------------------------
  
  if("distance" %in% info)
    popup <- paste(sep = "<br/>", popup, paste0(round(markers$distance, digits = 0), "km"))
  
  if("distance_m" %in% info)
    popup <- paste(sep = "<br/>", popup, paste0(round(markers$distance, digits = 0), "m"))
  
  
  # ----------------------------------------------------------------------------
  # cum_distance
  # ----------------------------------------------------------------------------
  
  if("cum_distance" %in% info)
    popup <- paste(sep = "<br/>", popup, paste0(round(markers$cum_distance, digits = 0), "km"))
  
  
  # ----------------------------------------------------------------------------
  # speed
  # ----------------------------------------------------------------------------
  
  if("speed" %in% info)
    popup <- paste(sep = "<br/>", popup, paste0(round(markers$speed, digits = 1), "km/h"))
  
  
  # ----------------------------------------------------------------------------
  # action: show (leg) targets
  # ----------------------------------------------------------------------------
  
  if("show_targets" %in% info){
    
    # -- actionLink
    helper <- function(x)
      lapply(x, function(x)
        paste(actionLink(inputId = ns(paste0("show_targets_", x)),
                         label = "Show",
                         onclick = paste0('Shiny.setInputValue(\"', ns("leg_targets"), '\", this.id, {priority: \"event\"})'))))
    
    popup <- paste(popup, paste(helper(markers$segment_id), "leg targets."), sep = "<br/>")
    
  }
  
  
  # ----------------------------------------------------------------------------
  # action: clear (leg) targets
  # ----------------------------------------------------------------------------

  if("clear_targets" %in% info){
    
    # -- actionLink
    helper <- function(x)
      lapply(x, function(x)
        paste(actionLink(inputId = ns(paste0("leg_target_", x)),
                         label = "Clear",
                         onclick = paste0('Shiny.setInputValue(\"', ns("clear_group"), '\", this.id, {priority: \"event\"})'))))
    
    popup <- paste(popup, paste(helper(markers$segment_id), "leg targets."), sep = "<br/>")
    
  }
  
  
  # ----------------------------------------------------------------------------
  # action: add (leg)
  # ----------------------------------------------------------------------------
  
  if("add_leg" %in% info){
    
    # -- actionLink
    helper <- function(x)
      lapply(x, function(x)
        paste(actionLink(inputId = ns(paste0("add_leg_", x)),
                         label = "Add",
                         onclick = paste0('Shiny.setInputValue(\"', ns("create_leg"), '\", this.id, {priority: \"event\"})'))))
    
    popup <- paste(popup, paste(helper(markers$segment_id), "leg."), sep = "<br/>")
    
  }
  
  
  # ----------------------------------------------------------------------------
  # action: remove (leg)
  # ----------------------------------------------------------------------------
  
  if("remove_leg" %in% info){
    
    # -- actionLink
    helper <- function(x)
      lapply(x, function(x)
        paste(actionLink(inputId = ns(paste0("remove_leg_", x)),
                         label = "Remove",
                         onclick = paste0('Shiny.setInputValue(\"', ns("drop_leg"), '\", this.id, {priority: \"event\"})'))))
    
    popup <- paste(popup, paste(helper(markers$segment_id), "leg."), sep = "<br/>")
    
  }
  
  
  # ----------------------------------------------------------------------------
  # update markers & return
  # ----------------------------------------------------------------------------
  
  markers |>
    mutate(popup = popup)
  
}
