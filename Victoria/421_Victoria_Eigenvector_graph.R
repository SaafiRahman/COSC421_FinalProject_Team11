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

# Calculate Eigenvector Centrality
if (ecount(g) > 0) {
  eigenvector_centrality <- eigen_centrality(g, directed = FALSE)$vector
  
  # Assign eigenvector centrality scores
  filtered_stops$eigenvector <- eigenvector_centrality[match(filtered_stops$StopID, V(g)$name)]
  stops_sf$eigenvector <- eigenvector_centrality[match(stops_sf$StopID, V(g)$name)]
  
  # Highlight top eigenvector centrality stops
  top_eigenvector_stops <- stops %>%
    mutate(eigenvector = eigenvector_centrality[match(StopID, V(g)$name)]) %>%
    arrange(desc(eigenvector)) %>%
    head(5)
  
  top_eigenvector_stops_sf <- stops_sf %>%
    filter(StopID %in% top_eigenvector_stops$StopID)
  
  # Visualize eigenvector centrality
  tmap_mode("view")
  eigenvector_map <- tm_shape(routes) +
    tm_lines(col = "blue", lwd = 2, alpha = 0.7) +
    tm_shape(stops_sf) +
    tm_dots(
      col = "eigenvector",
      palette = "YlGnBu",
      size = 0.1,
      scale = 0.5,
      alpha = 0.8,
      title = "Eigenvector Centrality"
    ) +
    tm_shape(top_eigenvector_stops_sf) +
    tm_text("StopName", size = 0.7, just = "left", bg.color = "white", bg.alpha = 0.8) +
    tm_layout(title = "Eigenvector Centrality with Top Nodes Highlighted")
  
  print(eigenvector_map)
  
  # Display the top 5 stops by eigenvector centrality
  cat("Top 5 Stops by Eigenvector Centrality:\n")
  print(top_eigenvector_stops %>% select(StopName, eigenvector))
  
} else {
  cat("No edges in the graph; centrality measures cannot be computed.\n")
}