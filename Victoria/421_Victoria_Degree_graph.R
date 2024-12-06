# Load required libraries
library(sf)
library(tidyverse)
library(igraph)
library(tmap)
library(dbscan)

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

# **Step 1: Cluster Nearby Stops within 100m**

# Transform coordinates to a projected CRS for accurate distance calculations (e.g., UTM)
stops_projected <- st_transform(stops_sf, crs = 26910)  # UTM Zone 10N

# Extract coordinates
coords <- st_coordinates(stops_projected)

# Use DBSCAN to cluster stops within a specified distance (e.g., 100 meters)
dbscan_result <- dbscan(coords, eps = 100, minPts = 1)

# Add cluster IDs to the stops
stops_projected$cluster_id <- dbscan_result$cluster

# **Step 2: Create Super-Nodes**

# Aggregate stops by cluster
super_stops <- stops_projected %>%
  group_by(cluster_id) %>%
  summarize(
    StopID = first(StopID),  # Representative StopID
    StopName = paste(unique(StopName), collapse = " / "),  # Combine names
    geometry = st_union(geometry)  # Combine geometries
  )

# For visualization, get centroid of the combined geometry
super_stops <- st_centroid(super_stops)

# **Step 3: Adjust Edge List**

# Map original stops to their clusters
stops_cluster_mapping <- stops_projected %>%
  select(OriginalStopID = StopID, cluster_id)

# Update the stops data with cluster IDs
stops <- stops %>%
  left_join(stops_cluster_mapping, by = c("StopID" = "OriginalStopID"))

# Adjust the edge list to use cluster IDs
edge_list <- stops %>%
  arrange(StopSequence) %>%
  mutate(next_cluster = lead(cluster_id)) %>%
  filter(!is.na(next_cluster)) %>%
  filter(cluster_id != next_cluster) %>%  # Remove self-loops
  select(cluster_id, next_cluster) %>%
  distinct()  # Remove duplicate edges

# Create the graph from the new edge list
g <- graph_from_data_frame(edge_list, directed = FALSE)

# **Step 4: Calculate Degree Centrality**

if (ecount(g) > 0) {
  degree_centrality <- degree(g, mode = "all")
  
  # Assign degree centrality scores to super-stops
  super_stops$degree <- degree_centrality[match(super_stops$cluster_id, V(g)$name)]
  
  # Highlight top degree centrality stops
  top_degree_stops <- super_stops %>%
    arrange(desc(degree)) %>%
    head(5)
  
  # **Step 5: Visualize Degree Centrality**
  
  tmap_mode("view")
  degree_map <- tm_shape(routes) +
    tm_lines(col = "blue", lwd = 2, alpha = 0.7) +
    tm_shape(super_stops) +
    tm_dots(
      col = "degree",
      palette = "YlOrRd",
      size = 0.1,
      scale = 0.5,
      alpha = 0.8,
      title = "Degree Centrality"
    ) +
    tm_shape(top_degree_stops) +
    tm_text("StopName", size = 0.7, just = "left", bg.color = "white", bg.alpha = 0.8) +
    tm_layout(title = "Degree Centrality with Top Nodes Highlighted")
  
  print(degree_map)
  
  # Display the top 5 stops by degree centrality
  cat("Top 5 Stops by Degree Centrality:\n")
  print(top_degree_stops %>% select(StopName, degree))
  
} else {
  cat("No edges in the graph; centrality measures cannot be computed.\n")
}
