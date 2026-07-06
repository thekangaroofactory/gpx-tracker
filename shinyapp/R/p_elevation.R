

p_elevation <- function(data){
  
  ggplot(data, 
         aes(x = cum_distance / 1000, y = elevation_end)) +
    
    # -- elevation profile
    geom_area() +
    
    # -- axis labels
    scale_x_continuous(labels = ~paste0(.x, "km")) +
    scale_y_continuous(labels = ~paste0(.x, "m")) +
    
    # -- apply theme
    plot_theme()
  
}
