> if (!require(ggplot2)) install.packages("ggplot2")
> if (!require(dplyr)) install.packages("dplyr")
> if (!require(sf)) install.packages("sf")
> if (!require(igraph)) install.packages("igraph")
> if (!require(dplyr)) install.packages("dplyr")

> bus_stops_path <- "path/to/bus_stops.shp"
> routes_path <- "path/to/routes.shp"
> bus_stops <- st_read(bus_stops_path)

> library(sf)
> library(dplyr)
> library(igraph)
> library(ggplot2)
> bus_stops_path <- "C:/Users/Dhruv/Downloads/Kamloops_stops/bus_stops.shp"
> routes_path <- "C:/Users/Dhruv/Downloads/Kamloops_routes/routes.shp"
> 
> bus_stops <- st_read(bus_stops_path)
> routes <- st_read(routes_path)

> glimpse(bus_stops)
> glimpse(routes)

> plot(routes$geometry, col = "blue", main = "Kamloops Transit Network", lwd = 2)
> bus_stop_coords <- st_coordinates(bus_stops)
> points(bus_stop_coords[, 1], bus_stop_coords[, 2], col = "red", pch = 16, cex = 1.5)
> edges <- routes %>%
+   st_coordinates() %>%
+   as.data.frame() %>%
+   group_by(L1) %>%
+   mutate(
+     from = lag(paste(X, Y, sep = ",")),
+     to = paste(X, Y, sep = ",")
+   ) %>%
+   filter(!is.na(from)) %>%
+   select(from, to)


> bus_stops_coords <- bus_stops %>%
+   mutate(
+     Coordinates = paste(stop_lon, stop_lat, sep = ",")
+   )
> 
> graph <- graph_from_data_frame(edges, directed = FALSE)
> degree_centrality <- degree(graph, normalized = TRUE)
> betweenness_centrality <- betweenness(graph, normalized = TRUE)
> eigenvector_centrality <- eigen_centrality(graph)$vector
> 
> centrality_data <- data.frame(
+   Coordinates = names(degree_centrality),
+   Degree_Centrality = degree_centrality,
+   Betweenness_Centrality = betweenness_centrality,
+   Eigenvector_Centrality = eigenvector_centrality
+ )
> centrality_results <- merge(bus_stops_coords, centrality_data, by = "Coordinates", all.x = TRUE)
> top_degree <- centrality_results %>%
+   arrange(desc(Degree_Centrality)) %>%
+   head(5) %>%
+   select(stop_name, Degree_Centrality)
> 
> top_betweenness <- centrality_results %>%
+   arrange(desc(Betweenness_Centrality)) %>%
+   head(5) %>%
+   select(stop_name, Betweenness_Centrality)
> 
> top_eigenvector <- centrality_results %>%
+   arrange(desc(Eigenvector_Centrality)) %>%
+   head(5) %>%
+   select(stop_name, Eigenvector_Centrality)
> top_centrality_table <- data.frame(
+   "Degree Centrality" = top_degree$stop_name,
+   "Degree Centrality Score" = top_degree$Degree_Centrality,
+   "Betweenness Centrality" = top_betweenness$stop_name,
+   "Betweenness Centrality Score" = top_betweenness$Betweenness_Centrality,
+   "Eigenvector Centrality" = top_eigenvector$stop_name,
+   "Eigenvector Centrality Score" = top_eigenvector$Eigenvector_Centrality
+ )
> print(top_centrality_table)

> bus_stops$lon <- st_coordinates(bus_stops)[, 1]
> bus_stops$lat <- st_coordinates(bus_stops)[, 2]
> 
> # Create a ggplot with routes and bus stops
> ggplot() +
+   geom_sf(data = routes, color = "blue", size = 1) + # Plot routes
+   geom_point(data = bus_stops, aes(x = lon, y = lat), color = "red", size = 2) + # Plot bus stops
+   labs(
+     title = "Kamloops Transit Network",
+     subtitle = "Routes (Blue) and Bus Stops (Red)",
+     x = "Longitude",
+     y = "Latitude"
+   ) +
+   theme_minimal()



> ggplot() +
+   geom_sf(data = routes, color = "gray70", size = 0.5) + # Plot routes
+   geom_point(data = centrality_results, aes(x = stop_lon, y = stop_lat, size = Degree_Centrality, color = Degree_Centrality), alpha = 0.8) + # Plot bus stops with size/color based on degree centrality
+   scale_color_gradient(low = "lightblue", high = "darkblue") + # Color gradient for centrality
+   labs(
+     title = "Degree Centrality of Kamloops Bus Stops",
+     subtitle = "Size and color represent the degree centrality",
+     x = "Longitude",
+     y = "Latitude"
+   ) +
+   theme_minimal() +
+   theme(
+     plot.title = element_text(size = 16, face = "bold"),
+     legend.title = element_text(size = 10)
+   )

> ggplot() +
+   geom_sf(data = routes, color = "gray70", size = 0.5) + # Plot routes
+   geom_point(data = centrality_results, aes(x = stop_lon, y = stop_lat, size = Betweenness_Centrality, color = Betweenness_Centrality), alpha = 0.8) + # Plot bus stops with size/color based on betweenness centrality
+   scale_color_gradient(low = "lightgreen", high = "darkgreen") + # Color gradient for centrality
+   labs(
+     title = "Betweenness Centrality of Kamloops Bus Stops",
+     subtitle = "Size and color represent the betweenness centrality",
+     x = "Longitude",
+     y = "Latitude"
+   ) +
+   theme_minimal() +
+   theme(
+     plot.title = element_text(size = 16, face = "bold"),
+     legend.title = element_text(size = 10)
+   )

> ggplot() +
+   geom_sf(data = routes, color = "gray70", size = 0.5) + # Plot routes
+   geom_point(data = centrality_results, aes(x = stop_lon, y = stop_lat, size = Eigenvector_Centrality, color = Eigenvector_Centrality), alpha = 0.8) + # Plot bus stops with size/color based on eigenvector centrality
+   scale_color_gradient(low = "lightcoral", high = "darkred") + # Color gradient for centrality
+   labs(
+     title = "Eigenvector Centrality of Kamloops Bus Stops",
+     subtitle = "Size and color represent the eigenvector centrality",
+     x = "Longitude",
+     y = "Latitude"
+   ) +
+   theme_minimal() +
+   theme(
+     plot.title = element_text(size = 16, face = "bold"),
+     legend.title = element_text(size = 10)
+   )

> ggplot() +
+   geom_sf(data = routes, color = "gray70", size = 0.5) + # Plot transit routes
+   geom_point(
+     data = centrality_results,
+     aes(x = stop_lon, y = stop_lat, size = Betweenness_Centrality),
+     color = "orange", # A single color for clarity
+     alpha = 0.7
+   ) +
+   scale_size_continuous(
+     range = c(2, 10), # Adjust point size range for better differentiation
+     name = "Betweenness Centrality"
+   ) +
+   labs(
+     title = "Betweenness Centrality of Kamloops Bus Stops",
+     subtitle = "Larger points indicate higher betweenness centrality",
+     x = "Longitude",
+     y = "Latitude"
+   ) +
+   theme_minimal() +
+   theme(
+     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
+     plot.subtitle = element_text(size = 12, hjust = 0.5),
+     axis.title = element_text(size = 12),
+     legend.title = element_text(size = 10),
+     legend.text = element_text(size = 8)
+   )

> ggplot() +
+   geom_sf(data = routes, color = "gray70", size = 0.5) + # Plot routes
+   geom_point(data = centrality_results, aes(x = stop_lon, y = stop_lat, size = Betweenness_Centrality, color = Betweenness_Centrality), alpha = 0.8) + # Plot bus stops with size/color based on betweenness centrality
+   scale_color_gradient(low = "lightgreen", high = "darkgreen") + # Color gradient for centrality
+   labs(
+     title = "Betweenness Centrality of Kamloops Bus Stops",
+     subtitle = "Size and color represent the betweenness centrality",
+     x = "Longitude",
+     y = "Latitude"
+   ) +
+   theme_minimal() +
+   theme(
+     plot.title = element_text(size = 16, face = "bold"),
+     legend.title = element_text(size = 10)
+   )

> communities <- cluster_louvain(graph)
> V(graph)$color <- rainbow(length(unique(membership(communities))))[membership(communities)]
> plot(graph, vertex.size = 5, vertex.label = NA, vertex.color = V(graph)$color,
+      main = "Communities in Kamloops Transit Network")
> save.image("C:\\Users\\Dhruv\\Downloads\\projectwip")
> bottom_degree <- centrality_results %>%
+   arrange(Degree_Centrality) %>%
+   head(5) %>%
+   select(stop_name, Degree_Centrality)
> 
> bottom_betweenness <- centrality_results %>%
+   arrange(Betweenness_Centrality) %>%
+   head(5) %>%
+   select(stop_name, Betweenness_Centrality)
> 
> bottom_eigenvector <- centrality_results %>%
+   arrange(Eigenvector_Centrality) %>%
+   head(5) %>%
+   select(stop_name, Eigenvector_Centrality)
> bottom_centrality_table <- data.frame(
+   "Degree Centrality" = bottom_degree$stop_name,
+   "Degree Centrality Score" = bottom_degree$Degree_Centrality,
+   "Betweenness Centrality" = bottom_betweenness$stop_name,
+   "Betweenness Centrality Score" = bottom_betweenness$Betweenness_Centrality,
+   "Eigenvector Centrality" = bottom_eigenvector$stop_name,
+   "Eigenvector Centrality Score" = bottom_eigenvector$Eigenvector_Centrality
+ )
> print(bottom_centrality_table)

> num_stops <- nrow(bus_stops)
> print(paste("Number of bus stops:", num_stops))

> num_routes <- nrow(routes)
> print(paste("Number of bus routes:", num_routes))

> num_communities <- length(unique(membership(communities)))
> print(paste("Number of communities:", num_communities))

> top_bottlenecks <- centrality_results %>%
+   arrange(desc(Betweenness_Centrality)) %>%
+   head(5)  # Top bottlenecks
> print(top_bottlenecks)

> ggplot(centrality_results, aes(x = stop_lon, y = stop_lat, size = Betweenness_Centrality)) +
+   geom_point(color = "red", alpha = 0.7) +
+   labs(title = "Bottleneck Stops in Kamloops Network", x = "Longitude", y = "Latitude") +
+   theme_minimal()

> low_degree_stops <- centrality_results %>%
+   arrange(Degree_Centrality) %>%
+   head(10)  # Lowest degree stops
> print(low_degree_stops)

> shortest_paths_matrix <- distances(graph, mode = "all")
> average_distances <- apply(shortest_paths_matrix, 1, mean, na.rm = TRUE)
> longest_paths <- apply(shortest_paths_matrix, 1, max, na.rm = TRUE)
> service_gap_stops <- which(average_distances > quantile(average_distances, 0.9))
> print(service_gap_stops)

