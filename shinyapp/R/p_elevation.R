

p_elevation <- function(data){
  
  highest <- data |> 
    filter(elevation_end == max(elevation_end))
  lowest <- data |> 
    filter(elevation_end == min(elevation_end))
  
  ggplot(data, 
         aes(x = cum_distance / 1000, y = elevation_end)) +
    
    # -- elevation profile
    geom_area(fill = "#b5c098") +
    
    annotate("point", x = highest$cum_distance / 1000, y = -5, shape = 24) +
    annotate("point", x = lowest$cum_distance / 1000, y = -5, shape = 25) +
    
    # -- axis labels
    scale_x_continuous(labels = ~paste0(.x, "km")) +
    scale_y_continuous(labels = ~paste0(.x, "m")) +
    
    # -- apply theme
    plot_theme()
  
}
