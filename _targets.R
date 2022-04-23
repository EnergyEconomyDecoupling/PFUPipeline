library(magrittr)
library(targets)
library(PFUDatabase)
# targets::tar_make() to run the pipeline
# targets::tar_make_clustermq(workers = 8) to execute across multiple cores.
# targets::tar_read(<<target_name>>) to view the results.
# targets::tar_destroy() to start over with everything,


# Custom parameters
# years <- 1960:2019                         # The years to be analyzed
years <- 1971:1972                         # The years to be analyzed

# countries <- c("BRA", "CAN", "CHN", "DEU", "ESP", "GBR", "GHA", "GRC", "HKG", "HND", "IDN", "JPN", "IND", "JOR", "KOR", "MEX", "NOR", "RUS", "USA", "WMB", "WAB", "ZAF")
# countries <- c("WMB")
# countries <- c("USA")
# countries <- c("FSU", "YGS")
# countries <- c("SUN", "YUG")
# countries <- c("YGS")
# countries <- setdiff(PFUWorkflow::canonical_countries, c("FSU", "FYG", "CIV")) |> as.character()
countries <- c("WMBK", "WABK", "ZAF")
# countries <- PFUWorkflow::canonical_countries %>% as.character()

additional_exemplar_countries <- "WRLD"

# Should we do a release of the results?
release <- FALSE

# End user-adjustable parameters.






# Set up for multithreaded work on the local machine.
future::plan(future.callr::callr)

# Set options for all targets.
targets::tar_option_set(
  storage = "worker", 
  retrieval = "worker"
)

# Pull in the pipeline
PFUDatabase::get_pipeline(countries = countries,
                          additional_exemplar_countries = additional_exemplar_countries,
                          years = years,
                          how_far = "all_targets",
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
                          release = release)

