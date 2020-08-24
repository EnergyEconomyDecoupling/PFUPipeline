# This script loads the functions for this drake repository by sourcing relevant files

# Setup
source("R/packages.R")                     # Load your packages, e.g. library(drake).
# source("R/path_functions.R")               # Define your custom code as a bunch of functions.

# Custom parameters
max_year <- 2017                           # The last year to be analyzed
countries <- c("World", "ESP", "GRC")      # The countries to be analyzed

# Create the drake plan, using the custom parameters
source("R/plan.R")                         
