################################################################################
#                               CONFIG FILE
################################################################################

# FMestre

# Clear environment to start fresh  
rm(list = ls())  

# Set working directory (for portability, using `here::here()`)
here::here()

# Functions --------------------------------------------------------------------

install_if_missing <- function(packages) {  
  to_install <- packages[!packages %in% installed.packages()[, "Package"]]  
  if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)  
}

# -----------------------------------------------------------------------------

# List of required packages  
required_packages <- c("igraph", "cheddar", "taxize")
install_if_missing(required_packages)  

# Load required packages dynamically
invisible(lapply(required_packages, function(pkg) {
  suppressMessages(suppressWarnings(library(pkg, character.only = TRUE)))
}))

# Clean up variables
rm(required_packages, install_if_missing)

# -------------------------------------------------------------------------------
# Required functions

plot3d_fw <- function(position_list, label_type = "all") {
  
  cd <- cheddar_list[[position_list]]
  g <- igraph_list[[position_list]]
  
  # Get node positions for 3D layout (based on trophic level)
  nodes <- V(g)$name
  trophic_levels <- data.frame(cheddar::TrophicLevels(cd, "TrophicLevel"))
  trophic_levels <- trophic_levels$LongestTL 
  
  # Generate random x and y positions for better visualization
  set.seed(42)
  positions <- data.frame(
    x = runif(length(nodes), min = -5, max = 5),  
    y = runif(length(nodes), min = -5, max = 5),  
    z = trophic_levels  # Use trophic level as height
  )
  
  # Define colors based on trophic level
  node_colors <- colorRampPalette(c("red", "orange", "yellow"))(max(trophic_levels))[trophic_levels]
  
  # Open 3D plot
  open3d()
  
  # Plot nodes
  spheres3d(positions$x, positions$y, positions$z, radius = 0.3, col = node_colors)
  
  # Plot edges
  edges <- get.edgelist(g)
  for (i in 1:nrow(edges)) {
    node1 <- which(nodes == edges[i, 1])
    node2 <- which(nodes == edges[i, 2])
    segments3d(
      c(positions$x[node1], positions$x[node2]), 
      c(positions$y[node1], positions$y[node2]), 
      c(positions$z[node1], positions$z[node2]), 
      col = "lightgreen", lwd = 1
    )
  }
  
  # **Labeling logic**
  if (label_type == "all") {
    text3d(positions$x, positions$y, positions$z + 0.5, texts = nodes, col = "white", cex = 0.7)
  } else if (label_type == "top") {
    # Identify top predators (nodes with no outgoing edges)
    top_predators <- nodes[degree(g, mode = "out") == 0]
    predator_indices <- which(nodes %in% top_predators)
    
    text3d(
      positions$x[predator_indices], 
      positions$y[predator_indices], 
      positions$z[predator_indices] + 0.5, 
      texts = nodes[predator_indices], col = "white", cex = 0.7
    )
  }
  
  # Set background color
  bg3d(color = "black")
  
  # Adjust camera angle
  view3d(theta = 30, phi = 30, zoom = 0.8)
  
  # Show interactive 3D plot
  rglwidget()
}

