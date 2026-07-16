

p_speed <- function(data){

  # -- compute max speed
  max_speed <- data |> filter(speed == max(speed))
  
  # -- compute x labels
  # plot based on segment_id, otherwise it will create lags in the display
  # because of the breaks
  q <- max(data$cum_distance) / 4
  x_labels <- paste0(round(c(0:4) * q, digits = 0), "km")
  
  # -- return
  ggplot(data, 
         aes(x = segment_id, 
             y = speed)) +
    
    # -- speed evolution
    geom_area(fill = "#b5c098") +
    
    # -- cumulative mean
    geom_line(mapping = aes(y = cummean_speed),
              color = "#8a8661") +
    
    # -- maximum speed
    annotate("point", x = max_speed$segment_id, y = -1, shape = 24) +
    
    # -- axis
    scale_y_continuous(labels = ~paste0(.x, "km/h")) +
    scale_x_continuous(breaks = c(0:4) * max(data$segment_id) / 4, labels = x_labels) +
    
    # -- apply theme
    plot_theme()
  
}
