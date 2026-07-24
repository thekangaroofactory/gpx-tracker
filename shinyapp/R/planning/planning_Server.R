

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

planning_Server <- function(id, segments, title) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # -- namespace
    ns <- session$ns
    
    
    # --------------------------------------------------------------------------
    # Compute stats
    # --------------------------------------------------------------------------
    
    # -- compute distance
    distance <- sum(segments$distance) / 1000
    
    # -- compute times
    time_elapsed <- difftime(max(segments$datetime_end), min(segments$datetime_start), units = "hours")
    nb_day <- trunc(distance / c(LEG_DISTANCE_MAX, LEG_DISTANCE_MIN))

    
    # --------------------------------------------------------------------------
    # Outputs
    # --------------------------------------------------------------------------
    
    warning("Check comment factoriser ça avec l'autre serveur!")
    
    # -- title
    output$title <- renderText(tail(unlist(strsplit(unlist(strsplit(title, split = ".", fixed = T))[1], "_")), 1))
    
    # -- GPS points
    output$nb_points <- renderText(nrow(segments) + 1)
    
    # -- times
    output$time_expected <- renderText(paste0(floor(time_elapsed), "h", floor((time_elapsed - floor(time_elapsed)) * 60), "min"))
    
    
    output$nb_day <- renderText(paste0(paste(nb_day, collapse = "/"), "-day"))
    
    # -- distance
    output$distance <- renderText(paste0(round(distance, digits = 1), "km"))
    
    
    # -- track map
    output$map <- renderLeaflet(m_track(segments))
    
    # -- return
    NULL
    
  })
}
