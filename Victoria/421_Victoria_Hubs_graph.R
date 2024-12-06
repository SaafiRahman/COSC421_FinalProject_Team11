# Load required libraries
library(sf)
library(tidyverse)
library(tmap)

# Load stops data
stops <- read.csv("Victoria_stops.csv")

# Rename columns for clarity
colnames(stops) <- c("Index", "StopID", "StopName", "Latitude", "Longitude", "StopSequence")

# Convert stops to spatial points
stops_sf <- st_as_sf(stops, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to projected CRS for accurate distance calculations
stops_projected <- st_transform(stops_sf, crs = 26910)  # UTM Zone 10N

# Calculate the distance matrix (in meters)
distance_matrix <- st_distance(stops_projected)

# Convert to a data frame for easier processing
distance_df <- as.data.frame(as.table(as.matrix(distance_matrix)))

# Rename columns for clarity
colnames(distance_df) <- c("Stop1", "Stop2", "Distance")

# Filter distances greater than 1000 meters and create a line object
long_distances <- distance_df %>%
  mutate(Distance = as.numeric(Distance)) %>%  # Convert "units" to numeric
  filter(Distance > 1000) %>%  # Filter distances greater than 1000 meters
  mutate(
    Stop1Name = stops$StopName[as.numeric(Stop1)],
    Stop2Name = stops$StopName[as.numeric(Stop2)],
    Stop1Geom = stops_sf$geometry[as.numeric(Stop1)],
    Stop2Geom = stops_sf$geometry[as.numeric(Stop2)]
  ) %>%
  rowwise() %>%
  mutate(Line = st_sfc(st_linestring(c(Stop1Geom, Stop2Geom)), crs = 26910)) %>%
  st_as_sf()  # Convert to sf object

# Transform back to WGS84 for visualization
long_distances <- st_transform(long_distances, crs = 4326)

# Graph lines connecting far apart stops
tmap_mode("plot")
line_map <- tm_shape(stops_sf) +
  tm_dots(size = 0.1, col = "blue", alpha = 0.7, title = "Stops") +
  tm_shape(long_distances) +
  tm_lines(col = "red", lwd = 2, title = "Far Apart Stops (>1000m)") +
  tm_layout(title = "Stops Farther Than 1000 Meters Apart")

print(line_map)
