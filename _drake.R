# This file serves the r_*() functions (e.g. r_make()) documented at
# https://books.ropensci.org/drake/projects.html#safer-interactivity # nolint
# and
# https://docs.ropensci.org/drake/reference/r_make.html

# Load your packages and supporting functions into your session.
# If you use supporting scripts like the ones below,
# you will need to supply them yourself. Examples:
# https://github.com/wlandau/drake-examples/tree/master/main/R

library(PFUWorkflow)

# Custom parameters
max_year <- 2019                         # The last year to be analyzed

# countries <- c("BRA", "CAN", "CHNM", "DEU", "ESP", "GBR", "GHA", "GRC", "HKG", "HND", "IDN", "JPN", "IND", "JOR", "KOR", "MEX", "NOR", "RUS", "USA", "WMBK", "WABK", "ZAF")
# countries <- c("WMBK")
# countries <- c("CHNM")
# countries <- c("USA")
# countries <- c("ZAF")
# countries <- c("GBR", "USA", "WMBK", "WABK")
# countries <- c("OAFR", "OAMR", "OASI")
# countries <- c("FSU", "YGS")
# countries <- c("SUN", "YUG")
# countries <- c("YGS")
# countries <- setdiff(PFUDatabase::canonical_countries, c("FSU", "FYG", "CIV")) |> as.character()
countries <- PFUDatabase::canonical_countries |> as.character()

additional_exemplars <- "WRLD"

# Create our drake plan
plan <- PFUWorkflow::get_plan(countries = countries, 
                              additional_exemplar_countries = additional_exemplars,
                              max_year = max_year,
                              iea_data_path = PFUSetup::get_abs_paths()[["iea_data_path"]],
                              country_concordance_path = PFUSetup::get_abs_paths()[["country_concordance_path"]],
                              phi_constants_path = PFUSetup::get_abs_paths()[["phi_constants_path"]],
                              ceda_data_folder = PFUSetup::get_abs_paths()[["ceda_data_folder"]],
                              machine_data_path = PFUSetup::get_abs_paths()[["machine_data_folder"]],
                              exemplar_table_path = PFUSetup::get_abs_paths()[["exemplar_table_path"]],
                              fu_analysis_folder = PFUSetup::get_abs_paths()[["fu_analysis_folder"]],
                              reports_source_folders = PFUSetup::get_abs_paths()[["reports_source_folders"]],
                              reports_dest_folder = PFUSetup::get_abs_paths()[["reports_dest_folder"]], 
                              pipeline_caches_folder = PFUSetup::get_abs_paths()[["pipeline_caches_folder"]],
                              pipeline_releases_folder = PFUSetup::get_abs_paths()[["pipeline_releases_folder"]], 
                              release = FALSE)




# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
options(clustermq.scheduler = "multicore") # For parallel computing.
drake::drake_config(
  plan, 
  # max_expand = 1 # Set the number of countries you want to analyze
  parallelism = "clustermq",
  jobs = 8
)
