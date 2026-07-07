

break_summary <- function(data, idle = 60, short = 10, long = 30){

  # -- turn params min into sec
  short <- short * 60
  long <- long * 60
  
  # -- compute breaks & type
  data |> 
    filter(time > idle) |>
    mutate(type = case_when(as.Date(datetime_start) != as.Date(datetime_end) & time > 4 * 3600 ~ "overnight",
                            time >= long ~ "long",
                            time > short & time < long ~ "medium",
                            .default = "short"))

}
