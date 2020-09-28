# This file serves the r_*() functions (e.g. r_make()) documented at
# https://books.ropensci.org/drake/projects.html#safer-interactivity # nolint
# and
# https://docs.ropensci.org/drake/reference/r_make.html

# Load your packages and supporting functions into your session.
# If you use supporting scripts like the ones below,
# you will need to supply them yourself. Examples:
# https://github.com/wlandau/drake-examples/tree/master/main/R

library(SEAPSUTWorkflow)

# Custom parameters
max_year <- 2017                           # The last year to be analyzed

# The countries with complete FU Analysis files c("World", "ESP", "PRT", "MEX", "GBR", "GHA", "CHN")
# All are passing through the drake workflow without error at this time.
# The countries to be analyzed, this should be read from Dropbox or the Exemplar_Table excel sheet

countries <- c("World", "ESP", "GHA", "MEX", "PRT", "GBR", "CHN", "HND", "USA")

# Create our drake plan
plan <- SEAPSUTWorkflow::get_plan(countries = countries, 
                                  max_year = max_year,
                                  iea_data_path = PFUSetup::get_abs_paths()[["iea_data_path"]], 
                                  exemplar_table_path = PFUSetup::get_abs_paths()[["exemplar_table_path"]], 
                                  fu_analysis_folder = PFUSetup::get_abs_paths()[["fu_analysis_folder"]], 
                                  reports_source_folders = PFUSetup::get_abs_paths()[["reports_source_folders"]], 
                                  reports_dest_folder = PFUSetup::get_abs_paths()[["reports_dest_folder"]])

# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
# options(clustermq.scheduler = "multicore") # For parallel computing.
drake::drake_config(
  plan, 
  # max_expand = 1 # Set the number of countries you want to analyze
  # parallelism = "clustermq",
  # jobs = 16
)
