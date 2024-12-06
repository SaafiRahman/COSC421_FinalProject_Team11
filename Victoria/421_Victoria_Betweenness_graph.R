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

# Perform Louvain clustering
if (ecount(g) > 0) {
  set.seed(123)
  communities <- cluster_louvain(g)
  
  # Ensure consistent types for StopID
  filtered_stops$StopID <- as.character(filtered_stops$StopID)
  stops_sf$StopID <- as.character(stops_sf$StopID)
  V(g)$name <- as.character(V(g)$name)
  
  # Match stops to graph vertices
  matched_indices <- match(filtered_stops$StopID, V(g)$name)
  
  # Calculate centrality measures
  betweenness_centrality <- betweenness(g, directed = FALSE, normalized = TRUE)
  
  # Assign betweenness centrality scores
  filtered_stops$betweenness <- betweenness_centrality[matched_indices]
  
  # Add community memberships
  filtered_stops$community <- communities$membership[matched_indices]
  
  # Merge with spatial stops data
  stops_sf <- left_join(stops_sf, filtered_stops, by = "StopID")
  stops_sf$community <- as.factor(stops_sf$community)
  stops_sf$betweenness <- betweenness_centrality[match(stops_sf$StopID, V(g)$name)]
  
  # Highlight top betweenness centrality stops
  top_betweenness_stops <- filtered_stops %>%
    arrange(desc(betweenness)) %>%
    head(5)
  
  top_betweenness_stops_sf <- stops_sf %>%
    filter(StopID %in% top_betweenness_stops$StopID)
  
  # Visualize communities
  tmap_mode("view")
  tm_shape(routes) +
    tm_lines(col = "blue", lwd = 2, alpha = 0.7) +
    tm_shape(stops_sf) +
    tm_dots(col = "community", palette = "Set3", size = 0.03, alpha = 0.8, title = "Community") +
    tm_layout(title = "Communities in the Network")
  
  # Visualize betweenness centrality
  tm_shape(routes) +
    tm_lines(col = "blue", lwd = 2, alpha = 0.7) +
    tm_shape(stops_sf) +
    tm_dots(
      col = "betweenness",
      palette = "PuRd",
      size = 0.1,
      scale = 0.5,
      alpha = 0.8,
      title = "Betweenness Centrality"
    ) +
    tm_shape(top_betweenness_stops_sf) +
    tm_text("StopName", size = 0.7, just = "left", bg.color = "white", bg.alpha = 0.8) +
    tm_layout(title = "Betweenness Centrality with Top Nodes Highlighted")
  
} else {
  cat("No edges in the graph; centrality measures cannot be computed.\n")
}