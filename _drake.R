# This file serves the r_*() functions (e.g. r_make()) documented at
# https://books.ropensci.org/drake/projects.html#safer-interactivity # nolint
# and
# https://docs.ropensci.org/drake/reference/r_make.html

# Load your packages and supporting functions into your session.
# If you use supporting scripts like the ones below,
# you will need to supply them yourself. Examples:
# https://github.com/wlandau/drake-examples/tree/master/main/R
source("R/init.R")

# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
# options(clustermq.scheduler = "multicore") # For parallel computing.
drake_config(
  plan, 
  # max_expand = 1 # Set the number of countries you want to analyze
  # parallelism = "clustermq",
  # jobs = 16
)
