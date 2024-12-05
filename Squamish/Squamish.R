.Rproj.user
.Rhistory
.RData
.Ruserdata

library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(sf)
library(geosphere)


routes <- read.csv('/Users/arnauantunezgonzalez/Desktop/Network Science/Project/Data Squamish/routes.csv')
shapes <- read.csv('/Users/arnauantunezgonzalez/Desktop/Network Science/Project/Data Squamish/shapes.csv')
stop_times <- read.csv('/Users/arnauantunezgonzalez/Desktop/Network Science/Project/Data Squamish/stop_times.csv')
stops <- read.csv('/Users/arnauantunezgonzalez/Desktop/Network Science/Project/Data Squamish/stops.csv')
trips <- read.csv('/Users/arnauantunezgonzalez/Desktop/Network Science/Project/Data Squamish/trips.csv')

# Merge trips with routes to get route-specific trips
route_trips <- merge(trips, routes, by = "route_id")
 
# Filter stop_times to include only the stops for trips in route_trips
route_trip_ids <- unique(route_trips$trip_id)
stop_times_filtered <- stop_times[stop_times$trip_id %in% route_trip_ids, ]
 
# Sort stop_times by trip_id and stop_sequence
stop_times_filtered <- stop_times_filtered[order(stop_times_filtered$trip_id, stop_times_filtered$stop_sequence), ]
# Create edges by linking each stop to the next stop in the sequence for each trip
stop_times_filtered$next_stop_id <- ave(stop_times_filtered$stop_id, stop_times_filtered$trip_id, FUN = function(x) c(x[-1], NA))
edges <- stop_times_filtered[!is.na(stop_times_filtered$next_stop_id), c("stop_id", "next_stop_id", "trip_id")]
 
# Add route_id to edges by merging with trips
edges <- merge(edges, trips[, c("trip_id", "route_id")], by = "trip_id")
 
# Filter stops to include only the ones that appear in edges
unique_stop_ids <- unique(c(edges$stop_id, edges$next_stop_id))
nodes <- stops[stops$stop_id %in% unique_stop_ids, ]
 
# Merge nodes with stop_times to assign each stop a route_id (first occurrence of route)
nodes <- merge(nodes, stop_times_filtered[, c("stop_id", "trip_id")], by = "stop_id", all.x = TRUE)
nodes <- merge(nodes, trips[, c("trip_id", "route_id")], by = "trip_id", all.x = TRUE)
nodes <- nodes[!duplicated(nodes$stop_id), c("stop_id", "route_id")]
 
g <- graph_from_data_frame(edges[, c("stop_id", "next_stop_id")], vertices = nodes, directed = FALSE)

# Load stops data with latitude and longitude
stops_sf <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326) # WGS84 coordinate system

# Merge edges with stop coordinates to get from and to points
edges_with_coords <- merge(edges, stops, by.x = "stop_id", by.y = "stop_id")
edges_with_coords <- merge(edges_with_coords, stops, by.x = "next_stop_id", by.y = "stop_id", suffixes = c("_from", "_to"))

# Create spatial lines for each edge
edges_sf <- st_sfc(lapply(1:nrow(edges_with_coords), function(i) {
  st_linestring(matrix(c(
    edges_with_coords$stop_lon_from[i], edges_with_coords$stop_lat_from[i],
    edges_with_coords$stop_lon_to[i], edges_with_coords$stop_lat_to[i]
  ), ncol = 2, byrow = TRUE))
}), crs = 4326)

# Convert edges to an sf object
edges_sf <- st_sf(geometry = edges_sf)

# Check if the disconnected stop appears in stop_times
disconnected_stops <- stops[!stops$stop_id %in% stop_times$stop_id, ]
# Add a new column to mark disconnected nodes
stops_sf$connection_status <- ifelse(stops_sf$stop_id %in% stop_times$stop_id, "Connected", "Disconnected")

# Merge edges with coordinates for both start and end points
edges_with_coords <- merge(edges, stops[, c("stop_id", "stop_lat", "stop_lon")], by.x = "stop_id", by.y = "stop_id")
edges_with_coords <- merge(edges_with_coords, stops[, c("stop_id", "stop_lat", "stop_lon")], by.x = "next_stop_id", by.y = "stop_id", suffixes = c("_from", "_to"))

# Step 2: Create LINESTRING geometry for each edge, and keep route_id
# Create an sf object where route_id is included along with geometry
edges_sf <- st_sf(
  route_id = edges_with_coords$route_id, # Ensure route_id is an attribute
  geometry = st_sfc(lapply(1:nrow(edges_with_coords), function(i) {
    st_linestring(matrix(c(
      edges_with_coords$stop_lon_from[i], edges_with_coords$stop_lat_from[i],
      edges_with_coords$stop_lon_to[i], edges_with_coords$stop_lat_to[i]
    ), ncol = 2, byrow = TRUE))
  }), crs = 4326)
)

route_colors <- c(
  "1-SQU" = "#FF5733", 
  "2-SQU" = "#33FF57",
  "3-SQU" = "#3357FF", 
  "4-SQU" = "#FF33A6",
  "9-SQU" = "#FFD700", 
  "NA" = "#808080"   
)

# Ensure `disconnected_stops` is an sf object with a geometry column
if (!inherits(disconnected_stops, "sf")) {
  disconnected_stops <- st_as_sf(disconnected_stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
}

# Check the structure to confirm it has the geometry column
print(st_geometry(disconnected_stops))

ggplot() +
  geom_sf(data = edges_sf, aes(color = factor(route_id)), size = 1, alpha = 0.6) + # Color edges by route
  geom_sf(data = stops_sf, color = "gray30", size = 2.5) + # Neutral color for regular nodes
  geom_sf(data = disconnected_stops, color = "red", size = 2.5) + # Highlight disconnected node in red with same size and shape
  scale_color_manual(values = route_colors) + # Apply vibrant color palette for routes
  labs(title = "Squamish Public Transit Network with Highlighted Disconnected Node", color = "Route ID") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


network_graph <- graph_from_data_frame(d = edges, directed = FALSE)
V(network_graph)$geometry <- stops_sf$geometry[match(V(network_graph)$name, stops$stop_id)]

node_degrees <- degree(network_graph)

stops_sf$degree_centrality <- node_degrees[as.character(stops_sf$stop_id)]

# Identify hubs (nodes with the highest degree)
max_degree <- max(node_degrees)
hubs <- which(node_degrees == max_degree)
cat("The node(s) with the most connections (hubs):", 
    V(network_graph)$name[hubs], 
    "with", max_degree, "connections.\n")


betweenness_values <- igraph::betweenness(network_graph, directed = FALSE, normalized = TRUE)

stops_sf <- stops_sf %>%
  filter(stop_id != 934000)

stops_sf$betweenness <- betweenness_values[match(stops_sf$stop_id, V(network_graph)$name)]

stops_sf$betweenness[is.na(stops_sf$betweenness)] <- 0

highest_betweenness <- max(stops_sf$betweenness, na.rm = TRUE)

stops_sf <- stops_sf %>%
  mutate(highlight = ifelse(betweenness == highest_betweenness, "blue", "gray30"))

eigenvector_centrality <- igraph::eigen_centrality(network_graph, directed = FALSE)$vector

stops_sf$eigenvector_centrality <- eigenvector_centrality[match(stops_sf$stop_id, V(network_graph)$name)]
stops_sf$eigenvector_centrality[is.na(stops_sf$eigenvector_centrality)] <- 0
top_betweenness <- head(sort(betweenness_values, decreasing = TRUE), 5)
cat("Top nodes by betweenness centrality:\n")
print(top_betweenness)

top_eigenvector <- head(sort(eigenvector_centrality, decreasing = TRUE), 5)
cat("Top nodes by eigenvector centrality:\n")
print(top_eigenvector)
# Summary 
cat("\nSummary of betweenness centrality in stops_sf:\n")
summary(stops_sf$betweenness)
cat("\nSummary of eigenvector centrality in stops_sf:\n")
summary(stops_sf$eigenvector_centrality)

hits_result <- hits_scores(g, scale = TRUE)
V(g)$hub_score <- hits_result$hub  # Assign hub scores to the graph vertices
stops_sf$hub_score <- hits_result$hub[match(stops_sf$stop_id, V(g)$name)]
threshold <- 0.8
high_hub_nodes <- subset(stops_sf, hub_score >= threshold)
cat("High hub nodes (hub score >= threshold):\n")
print(high_hub_nodes)

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Ensure CRS matches
stops_sf <- st_transform(stops_sf, st_crs(edges_sf))

# Step 2: Perform spatial join to associate stops with routes
stops_df_with_routes <- st_join(stops_sf, edges_sf, join = st_intersects)

# Step 3: Aggregate multiple route IDs per stop (if required)
stops_df_with_routes <- stops_df_with_routes %>%
  group_by(stop_id) %>%
  summarise(
    geometry = first(geometry), # Retain the original stop geometry
    route_ids = paste(unique(route_id), collapse = ", ") # Combine route IDs
  )

# Step 4: Convert aggregated routes to sf
stops_sf_with_routes <- st_as_sf(stops_df_with_routes, crs = st_crs(stops_sf))

# Step 5: Split combined `route_ids` into separate rows
stops_sf_with_routes_expanded <- stops_sf_with_routes %>%
  separate_rows(route_ids, sep = ", ") %>%
  rename(route_id = route_ids) # Rename column to `route_id`

# Step 6: Function to find the nearest stop with shared routes
find_nearest_stop_shared_routes <- function(stop_id, route_id, stops_sf_with_routes_expanded) {
  # Filter stops on the same route
  shared_stops <- stops_sf_with_routes_expanded %>%
    filter(route_id == !!route_id)
  
  # Calculate distances to all shared stops
  distances <- as.numeric(st_distance(
    stops_sf_with_routes_expanded[stops_sf_with_routes_expanded$stop_id == stop_id, ],
    shared_stops
  ))
  
  # Exclude the current stop itself
  distances[distances == 0] <- Inf
  
  # Find the nearest stop
  nearest_idx <- which.min(distances)
  if (length(nearest_idx) == 0 || distances[nearest_idx] == Inf) return(NA)
  return(shared_stops$stop_id[nearest_idx])
}

# Step 7: Calculate nearest stop for each stop based on shared routes
stops_sf_with_routes_expanded$nearest_stop <- mapply(
  find_nearest_stop_shared_routes,
  stop_id = stops_sf_with_routes_expanded$stop_id,
  route_id = stops_sf_with_routes_expanded$route_id,
  MoreArgs = list(stops_sf_with_routes_expanded = stops_sf_with_routes_expanded)
)

# Step 8: Optional: Calculate distances to the nearest stop
stops_sf_with_routes_expanded$nearest_distance <- mapply(function(stop_id, route_id) {
  nearest_id <- stops_sf_with_routes_expanded$nearest_stop[
    stops_sf_with_routes_expanded$stop_id == stop_id & stops_sf_with_routes_expanded$route_id == route_id
  ]
  if (is.na(nearest_id)) return(NA)
  as.numeric(st_distance(
    stops_sf_with_routes_expanded[stops_sf_with_routes_expanded$stop_id == stop_id & stops_sf_with_routes_expanded$route_id == route_id, ],
    stops_sf_with_routes_expanded[stops_sf_with_routes_expanded$stop_id == nearest_id & stops_sf_with_routes_expanded$route_id == route_id, ]
  ))
}, stop_id = stops_sf_with_routes_expanded$stop_id, route_id = stops_sf_with_routes_expanded$route_id)

# Step 9: Create line segments connecting stops to their nearest stops
nearest_lines <- lapply(1:nrow(stops_sf_with_routes_expanded), function(i) {
  nearest_id <- stops_sf_with_routes_expanded$nearest_stop[i]
  if (is.na(nearest_id)) return(NULL) # Skip if no nearest stop
  
  # Extract the two stops to connect
  stop1 <- stops_sf_with_routes_expanded[i, ]
  stop2 <- stops_sf_with_routes_expanded[
    stops_sf_with_routes_expanded$stop_id == nearest_id & stops_sf_with_routes_expanded$route_id[i] == stops_sf_with_routes_expanded$route_id, 
  ]
  
  if (nrow(stop2) == 0) return(NULL) # Skip if the nearest stop is not found
  
  # Create a line connecting the two stops
  return(st_linestring(matrix(c(
    st_coordinates(stop1),
    st_coordinates(stop2)
  ), ncol = 2, byrow = TRUE)))
})

# Remove NULL entries
nearest_lines <- nearest_lines[!sapply(nearest_lines, is.null)]

# Convert to an sfc object
nearest_lines_sfc <- st_sfc(nearest_lines, crs = st_crs(stops_sf_with_routes_expanded))

# Create an sf object for the lines
nearest_lines_sf <- st_as_sf(data.frame(geometry = nearest_lines_sfc), crs = st_crs(stops_sf_with_routes_expanded))

# Step 10: Visualization
ggplot() +
  geom_sf(data = stops_sf_with_routes_expanded, aes(color = route_id), size = 2.5) + # Stops
  geom_sf(data = nearest_lines_sf, color = "blue", size = 0.5) + # Connections
  labs(
    title = "Nearest Stops with Shared Routes",
    color = "Route ID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


stops_sf_with_routes$eigen_centrality <- eigen_centrality_values[V(g)$name %in% stops_sf_with_routes$stop_id]

# Recalculate eigenvector centrality
eigen_centrality_values <- eigen_centrality(g, directed = FALSE)$vector
# Map centrality values to stops
eigen_centrality_named <- setNames(eigen_centrality_values, V(g)$name)
stops_sf_with_routes$eigen_centrality <- eigen_centrality_named[stops_sf_with_routes$stop_id]
 
# Replace NA values with 0
stops_sf_with_routes$eigen_centrality[is.na(stops_sf_with_routes$eigen_centrality)] <- 0

stops_sf_with_routes$eigen_centrality <- eigen_centrality_values[match(
       stops_sf_with_routes$stop_id,
       names(eigen_centrality_values)
)]

ggplot() +
  # Edges: Color mapped to route_id
  geom_sf(data = edges_sf, aes(color = factor(route_id)), size = 0.5, alpha = 0.6, inherit.aes = FALSE) +
  
  # Nodes: Size and color based on eigenvector centrality
  geom_sf(
    data = stops_sf_with_routes,
    aes(size = eigen_centrality, fill = eigen_centrality),
    shape = 21, color = "black", stroke = 0.3
  ) +
  
  # Continuous color scale for eigenvector centrality
  scale_fill_viridis_c(
    option = "plasma",
    name = "Eigenvector\nCentrality"
  ) +
  
  # Continuous size scale for nodes
  scale_size_continuous(
    range = c(2, 8),
    name = "Node Size\n(Centrality)"
  ) +
  
  # Manual color scale for edges based on routes
  scale_color_manual(
    values = route_colors,
    name = "Route ID"
  ) +
  
  # Labels and themes
  labs(
    title = "Transit Network: Eigenvector Centrality",
    subtitle = "Node size and color represent eigenvector centrality values",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right"
  )

