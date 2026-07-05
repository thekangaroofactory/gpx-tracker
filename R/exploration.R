

# -- data source
url <- file.path("E:/Datasets/gpx-tracker", "2026-06-20_3055267978_Charenton à Troyes.gpx")
url <- file.path("E:/Datasets/gpx-tracker", "2026-06-30_3074592271_Boucle La Teste _ Arcachon _ Pilat _ Cazaux.gpx")
url <- file.path("E:/Datasets/gpx-tracker", "2024-08-10_1778425758_Chicago Lakefront Trail.gpx")


# -- load track
track <- sf::read_sf(url, layer = "track_points")
cols <- c("track_seg_point_id", "ele", "time", "geometry")
track <- track[cols]

# -- extract long / lat from geometry

track <- track |>
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2])


# -- compute time difference between points
# help find breaks > threshold needed to tune real breaks from random stop...
# order desc gives list of pauses
# row before bigger values is when pause started
track <- track |> mutate(difftime = time - lag(time))

# -- compute elevation difference
track <- track |> mutate(elevation_gain = ele - lag(ele))

# -- total positive gain
sum(track |>
      filter(elevation_gain > 0) |>
      pull(elevation_gain))

# -- total negative gain
sum(track |>
      filter(elevation_gain < 0) |>
      pull(elevation_gain))

# -- elevation min / max
min(track$ele)
max(track$ele)

# -- elevation evolution
ggplot2::ggplot(track, ggplot2::aes(x = track_seg_point_id, y = ele)) +
  ggplot2::geom_area()


# -- Activity time
# note: need to manage minutes...
difftime(max(track$time), min(track$time), units = "hours")


# -- compute distances
track <- track |>
  mutate(dist = st_distance(lag(geometry), geometry, by_element = T))

# -- total distance (km)
sum(track$dist, na.rm = T) / 1000


# -- compute speed
track <- track |>
  mutate(speed = dist / as.numeric(difftime) / 1000 * 60 * 60)

# -- area plot
# note: a boxplot is also nice in addition to this one
ggplot2::ggplot(track, ggplot2::aes(x = track_seg_point_id, y = speed)) +
  ggplot2::geom_area()

# Komoot they compute mean speed as total_distance / activity_time
# it gives a lower value compared to mean of the speed column
# I think it should be named theoric mean speed as it's not computed as a mean

# -- mean speed
# I believe median speed is a better metric
mean(track$speed, na.rm = T)
median(track$speed, na.rm = T)

# -- at some point, drop the first row as no stats (cummean will output NAs)
track2 <- track[-1,]
track2 <- track2 |> mutate(cummean_speed = cummean(speed))

# -- cummean evolution
# note: maybe mix this with some rolling window mean speed
ggplot2::ggplot(track2, ggplot2::aes(x = track_seg_point_id, y = cummean_speed)) +
  ggplot2::geom_line()


# -- compute activity time
# Seems like they do it based on filtered rows with difftime < ~ 20sec
# 13.09394 sec = 13h 5.4min (against 13h04 on Komoot)
# Check if we could mix filters (difftime, distance & speed?)
# in any case, it's an approximation, so the mean speed computed on that is an approximation as well
# distance is not easy because gps coordinates have a 5/10m precision window
# Maybe check if points could be grouped to compute an average position for that group... tbc with a plot on map

track3 <- track2 |>
  filter(difftime < 20)

sum(track3$difftime) / 3600


# -- check overnight
# add date column and check between two points + difftime
# or get longest difftime / pause + check dates


# -- map data
leaflet::leaflet(data = track3) %>%
  leaflet::addTiles() %>%
  leaflet::addPolylines(lng = ~lon, lat = ~lat, weight = 2, color = "black", opacity = 1) %>%
  leaflet::addCircleMarkers(data = track2 |> filter(difftime > 60), lng = ~lon, lat = ~lat, radius = ~difftime/1000)
