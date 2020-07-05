# This script loads the functions for this drake repository by sourcing relevant files

# Setup
source("R/packages.R")                     # Load your packages, e.g. library(drake).
source("R/functions/path_functions.R")     # Define your custom code as a bunch of functions.

countries <- c("ESP")
max_year <- 2017
paths <- get_abs_paths()

source("R/plan.R")                         # Create the drake plan.
