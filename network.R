# Load necessary libraries
install.packages('igraph')
install.packages('ggraph')

# Load necessary libraries
library(igraph)
library(ggraph)

# Assuming the data is already loaded as 'newham_pub'
# Subset the top 30 rows of the data
newham_pub_top30 <- head(newham_pub, 30)

# Create a graph object from the subset data
graph <- graph_from_data_frame(d = newham_pub_top30, directed = FALSE)

# Check the names of the nodes in the graph
node_names <- V(graph)$name

# Ensure the "Cultural.Venue.Type" is aligned with the node names
color_mapping <- setNames(as.factor(newham_pub_top30$Cultural.Venue.Type), newham_pub_top30$Venue.Name)
V(graph)$color <- color_mapping[node_names]

# Perform clustering (e.g., community detection)
clusters <- cluster_louvain(graph)

# Assign cluster membership to nodes
V(graph)$cluster <- membership(clusters)

# Define a custom color palette
custom_colors <- c('red', 'blue', 'green', 'purple', 'orange', 'pink', 'yellow', 'brown')

ggraph(graph, layout = 'fr') + 
  geom_edge_arc(aes(alpha = 0.3), color = "white") +  # Use curved edges
  geom_node_point(aes(color = factor(V(graph)$color)), size = 5) + 
  scale_color_manual(values = custom_colors) + 
  theme_void() + 
  ggtitle("Your Local Network of Safe Spots") + 
  theme(
    plot.title = element_text(hjust = 0.5, color = "white", size = 16),
    plot.background = element_rect(fill ="#041a40", color = NA),
    legend.position = "right",
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white")
  )


