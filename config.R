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