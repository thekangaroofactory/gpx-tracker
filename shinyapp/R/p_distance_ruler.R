

p_distance_ruler <- function(data, overnight = NULL){
  
  # -- outlier threshold
  # x <- summary(data$time)[c('1st Qu.', '3rd Qu.')]
  # time_ceiling <- x[2] + 1.5 * (x[2] - x[1])
  
  # -- compute axis breaks
  axis_x_breaks <- c(min(data$datetime_start), data$datetime_end)
  
  # -- init plot
  p <- ggplot(data, 
              mapping = aes(x = datetime_end)) +
    
    geom_segment(x = min(data$datetime_start), 
                 y = 0, 
                 xend = max(data$datetime_end), 
                 yend = 0,
                 lineend = "round",
                 linewidth = 4,
                 colour = "#b5c098")
  
  # -- add overnight time
  if(is.data.frame(overnight) && nrow(overnight) > 0)
    p <- p + geom_segment(x = overnight$datetime_start,
                          xend = overnight$datetime_end,
                          y = 0,
                          yend = 0,
                          lineend = "round",
                          linewidth = 3,
                          colour = "#d8dec9")
  
  # -- add slowest / fastest sections
  p <- p + geom_segment(data = data |>
                          filter(time == max(time)),
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
                 colour = "green")
  
  # -- add section markers
  p <- p + geom_point(aes(y = 0.1), shape = 25) +
    geom_text(mapping = aes(label = round(cum_distance, digits = 0)), y = 0.2) +
    annotate("point", x = min(data$datetime_start), y = 0.1, shape = 25) +
    annotate("text", x = min(data$datetime_start), y = 0.2, label = "0")
  
  # -- scale & theme
  p + scale_x_datetime(breaks = axis_x_breaks, labels = ~format(axis_x_breaks, "%Hh%M"), limits = c(min(axis_x_breaks), NA)) +
    ylim(c(0, 0.5)) +
    
    plot_theme() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  
}
