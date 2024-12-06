# Load required libraries
library(sf)
library(tidyverse)
library(igraph)
library(tmap)

# Load routes shapefile
routes <- st_read("routes.shp")

# Load stops data
stops <- read.csv("Victoria_stops.csv")

# Rename columns for clarity
colnames(stops) <- c("Index", "StopID", "StopName", "Latitude", "Longitude", "StopSequence")

# Convert stops to spatial points
stops_sf <- st_as_sf(stops, coords = c("Longitude", "Latitude"), crs = 4326)

# Validate and transform CRS to match between routes and stops
if (st_crs(routes) != st_crs(stops_sf)) {
  routes <- st_transform(routes, crs = st_crs(stops_sf))
}

# Create edge list without RouteID
filtered_stops <- stops %>%
  arrange(StopSequence) %>%
  mutate(next_stop = lead(StopID)) %>%
  filter(!is.na(next_stop)) %>%
  select(StopID, next_stop)

# Create the graph from the edge list
g <- graph_from_data_frame(filtered_stops, directed = FALSE)

# Add stop information as node attributes
V(g)$StopID <- as.character(filtered_stops$StopID[match(V(g)$name, filtered_stops$StopID)])
V(g)$StopName <- stops$StopName[match(V(g)$StopID, stops$StopID)]

# Perform Louvain Clustering
if (ecount(g) > 0) {
  set.seed(123)
  communities <- cluster_louvain(g)
  
  # Assign community memberships to stops
  filtered_stops$community <- communities$membership[match(filtered_stops$StopID, V(g)$name)]
  stops_sf$community <- as.factor(communities$membership[match(stops_sf$StopID, V(g)$name)])
  
  # Visualize communities
  tmap_mode("view")  # Switch to plot mode for static maps
  community_map <- tm_shape(routes) +
    tm_lines(col = "blue", lwd = 2, alpha = 0.7) +
    tm_shape(stops_sf) +
    tm_dots(
      col = "community",
      palette = "Set3",
      size = 0.07,
      alpha = 0.8,
      title = "Community"
    ) +
    tm_layout(title = "Communities in the Network")
  
  print(community_map)  # Ensure the map is printed explicitly
  
  # Print number of communities
  cat("Number of Communities Detected:", length(unique(communities$membership)), "\n")
} else {
  cat("No edges in the graph; communities cannot be computed.\n")
}
