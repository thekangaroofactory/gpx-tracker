

p_distance_ruler <- function(data, overnight = NULL){
  
  # -- outlier threshold
  x <- summary(data$time)[c('1st Qu.', '3rd Qu.')]
  time_ceiling <- x[2] + 1.5 * (x[2] - x[1])
  
  # -- build plot
  ggplot(data, 
         mapping = aes(x = datetime_end)) +
    
    geom_segment(x = min(data$datetime_start), 
                 y = 0, 
                 xend = max(data$datetime_end), 
                 yend = 0,
                 lineend = "round",
                 linewidth = 4,
                 colour = "#b5c098") +
    
    # -- add overnight time
    geom_segment(x = overnight$datetime_start,
                 xend = overnight$datetime_end,
                 y = 0,
                 yend = 0,
                 lineend = "round",
                 linewidth = 3,
                 colour = "#d8dec9") +
    
    geom_segment(data = data |>
                   filter(time >= time_ceiling),
                 aes(
                   x = datetime_start, 
                   xend = datetime_end), 
                 y = 0, 
                 yend = 0,
                 lineend = "round",
                 linewidth = 3,
                 colour = "orange") +
    
    geom_segment(data = data |>
                   filter(time == min(time)),
                 aes(
                   x = datetime_start, 
                   xend = datetime_end), 
                 y = 0, 
                 yend = 0,
                 lineend = "round",
                 linewidth = 3,
                 colour = "green") +
    
    
    geom_point(aes(y = 0.1), shape = 25) +
    geom_text(mapping = aes(label = round(distance, digits = 0)), y = 0.2) +
    
    scale_x_continuous(breaks = data$datetime_end, labels = ~format(data$datetime_end, "%Hh%M")) +
    ylim(c(0, 0.5)) +
    
    plot_theme() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  
}
