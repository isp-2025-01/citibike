library(tidyverse)
library(sf)


list_of_files <- list.files(path = "202406-citibike-tripdata",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

june <- readr::read_csv(list_of_files, id = "file_name")

list_of_files <- list.files(path = "202412-citibike-tripdata",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)


dec <- readr::read_csv(list_of_files, id = "file_name")

june_agg <- june %>%
  mutate(
    time_interval = floor_date(started_at, "10 minutes") # Aggregate by 10-minute intervals
  ) %>%
  group_by(start_station_name, start_lat, start_lng, time_interval) %>%
  summarise(
    rides = n(), # Count the number of rides per station per interval
    .groups = "drop"
  )

# Convert to an sf object for spatial visualization
june_sf <- st_as_sf(june_agg, coords = c("start_lng", "start_lat"), crs = 4326)

# Create the base plot
p <- ggplot(june_sf) +
  geom_sf(aes(size = rides, color = rides), alpha = 0.7) +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Citi Bike Rides in June 2024",
    subtitle = "Time: {frame_time}",
    size = "Number of Rides",
    color = "Number of Rides"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Animate the plot
animated_plot <- p +
  transition_time(time_interval) +
  ease_aes("linear")

# Render the animation
animate(animated_plot, nframes = 200, fps = 10, width = 800, height = 600)

anim_save("citibike_june_2024.gif", animation = animated_plot, nframes = 200, fps = 10)
