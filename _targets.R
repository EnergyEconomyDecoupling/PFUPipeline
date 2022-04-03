library(magrittr)
library(targets)
# targets::tar_make() to run the pipeline
# targets::tar_make_clustermq(workers = 8) to execute across multiple cores.
# targets::tar_read(<<target_name>>) to view the results.
# targets::tar_destroy() to start over with everything,


# Custom parameters
years <- 2019                         # The years to be analyzed

# countries <- c("BRA", "CAN", "CHN", "DEU", "ESP", "GBR", "GHA", "GRC", "HKG", "HND", "IDN", "JPN", "IND", "JOR", "KOR", "MEX", "NOR", "RUS", "USA", "WMB", "WAB", "ZAF")
# countries <- c("WMB")
# countries <- c("USA")
# countries <- c("FSU", "YGS")
# countries <- c("SUN", "YUG")
# countries <- c("YGS")
# countries <- setdiff(PFUWorkflow::canonical_countries, c("FSU", "FYG", "CIV")) |> as.character()
countries <- setdiff(PFUWorkflow::canonical_countries, c("WRLD")) |> as.character()

additional_exemplars <- "WRLD"

# Number of machine cores to use.
# Set to less than available on your machine.
# Applies only to tar_make_clustermq().
# To parallelize the execution of this pipeline, say
# targets::tar_make_clustermq(workers = X),
# where X is the same as the number of cores.
# num_cores <- 3
num_cores <- 8

# Set the target to debug.  Set to NULL to turn off debugging.
# To debug, set appropriate breakpoints and use
# tar_make(callr_function = NULL).
# debug_target <- "PSUT_Re_all_St_fu"
debug_target <- NULL

# Should we do a release of the results?
release <- FALSE

# End user-adjustable parameters.


# Set up for multithreaded work on the local machine.
options(clustermq.scheduler = "multiprocess")

# Set options for the targets package.
targets::tar_option_set(
  
  # Set the target to debug, if needed.
  debug = debug_target,
  
  # Set packages to be used.
  packages = c(
    "dplyr",
    "IEATools",
    "parsedate",
    "PFUAggDatabase",
    "pins",
    "tidyr"),
  
  # Set the number of cores for multiprocessing.
  resources = targets::tar_resources(
    clustermq = targets::tar_resources_clustermq(template = list(num_cores = num_cores))
  )
)


# Pull in the pipeline
PFUDatabase::get_pipeline(countries = countries,
                          years = years,
                          aggregation_maps_path = PFUSetup::get_abs_paths()[["aggregation_mapping_path"]],
                          pipeline_caches_folder = PFUSetup::get_abs_paths()[["pipeline_caches_folder"]],
                          pipeline_releases_folder = PFUSetup::get_abs_paths()[["pipeline_releases_folder"]],
                          release = release)
