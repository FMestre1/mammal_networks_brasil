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
required_packages <- c("igraph", "cheddar", "taxize", "rgl", "NetIndices")
install_if_missing(required_packages)  

# Load required packages dynamically
invisible(lapply(required_packages, function(pkg) {
  suppressMessages(suppressWarnings(library(pkg, character.only = TRUE)))
}))

# Clean up variables
rm(required_packages, install_if_missing)

# -------------------------------------------------------------------------------
# Required functions

plot3d_fw <- function(position_list, label_type = "all", igraph_list, cheddar_list) {
  
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
  edges <- as_edgelist(g)
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

# Power-law function to fit
power_law_func <- function(k, a, b, c) {
  a * (k ^ b) + c
}

# Gaussian function to fit
gaussian_func <- function(k, a, b, c) {
  a * exp(-((k - b)^2) / (2 * c^2))
}

# Function to fit power-law or gaussian and compute Pearson R
fit_distribution <- function(degree_values, degree_freq, dist_type = c("power_law", "gaussian")) {
  dist_type <- match.arg(dist_type)
  
  # Objective function: sum of squared residuals for Nelder-Mead
  ssr <- function(params) {
    a <- params[1]
    b <- params[2]
    c <- params[3]
    fitted <- if (dist_type == "power_law") {
      power_law_func(degree_values, a, b, c)
    } else {
      gaussian_func(degree_values, a, b, c)
    }
    sum((degree_freq - fitted)^2)
  }
  
  # Initial parameter guesses
  init_params <- c(a=1, b= -1, c=0.1)
  if (dist_type == "gaussian") init_params <- c(a=max(degree_freq), b=mean(degree_values), c=sd(degree_values))
  
  # Optimization (Nelder-Mead)
  opt <- optim(init_params, ssr, method="Nelder-Mead")
  
  a <- opt$par[1]
  b <- opt$par[2]
  c <- opt$par[3]
  
  fitted <- if (dist_type == "power_law") {
    power_law_func(degree_values, a, b, c)
  } else {
    gaussian_func(degree_values, a, b, c)
  }
  
  # Pearson correlation coefficient R between observed freq and fitted
  R <- cor(degree_freq, fitted)
  
  list(params=opt$par, fitted=fitted, R=R)
}

# Main function to compare degree distribution of igraph networks list
compare_degree_distributions <- function(g_list) {
  results <- lapply(g_list, function(g) {
    deg <- degree(g, mode="all")
    deg_tab <- table(deg)
    degree_values <- as.numeric(names(deg_tab))
    degree_freq <- as.numeric(deg_tab) / sum(deg_tab)  # relative frequency
    
    pl_fit <- fit_distribution(degree_values, degree_freq, "power_law")
    gauss_fit <- fit_distribution(degree_values, degree_freq, "gaussian")
    
    c(RG = gauss_fit$R, RPL = pl_fit$R)
  })
  
  # combine into a dataframe
  results_df <- do.call(rbind, results)
  rownames(results_df) <- seq_along(g_list)
  return(as.data.frame(results_df))
}

