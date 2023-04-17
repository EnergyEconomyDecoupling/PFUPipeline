library(magrittr)
library(targets)
library(PFUDatabase)
# targets::tar_make() to run the pipeline
# targets::tar_make_future(workers = 8) to execute across multiple cores.
# targets::tar_read(<<target_name>>) to view the results.
# targets::tar_destroy() to start over with everything.
# targets::tar_make(callr_function = NULL) to debug.


# Version
version <- "v1.1"

# Custom parameters
years <- 1960:2020                        # The years to be analyzed
years <- 2019:2020
# years <- 1971
# years <- 1971:1980

countries <- "ARM"
# countries <- "USA"
# countries <- "GHA"
# countries <- "COL"
# countries <- "AGO"
# countries <- c("AGO", "COL")
# countries <- "WRLD"
# countries <- c(PFUDatabase::canonical_countries, "WRLD") %>% as.character()

# Countries with unique allocation data.
# countries <- c("BRA", "CAN", "CHNM", "DEU", "DNK", "ESP", "FRA", "GBR", "GHA",
#                "GRC", "HKG", "HND", "IDN", "IND", "JOR", "JPN", "KOR", "MEX",
#                "NOR", "PRT", "RUS", "USA", "WABK", "WMBK", "ZAF")

# Additional exemplar countries are countries which aren't included in the workflow
# as individual countries, but from which allocation or efficiency data may be 
# obtained and assigned to countries in the workflow using the exemplar system.
additional_exemplar_countries <- c("WRLD", # World
                                   "AFRI", # Africa 
                                   "ASIA", # Asia
                                   "EURP", # Europe 
                                   "MIDE", # Middle East
                                   "NAMR", # North America
                                   "OCEN", # Oceania 
                                   "SAMR", # South America 
                                   "BUNK") # Bunkers

# Should we specify non-energy flows?
specify_non_energy_flows <- TRUE

# Should we apply fixes to the IEA data?
apply_fixes <- TRUE

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
                          specify_non_energy_flows = specify_non_energy_flows,
                          apply_fixes = apply_fixes,
                          years = years,
                          how_far = "all_targets",
                          iea_data_path = PFUSetup::get_abs_paths(version = version)[["iea_data_path"]],
                          country_concordance_path = PFUSetup::get_abs_paths(version = version)[["country_concordance_path"]],
                          mw_concordance_path = PFUSetup::get_abs_paths(version = version)[["mw_concordance_path"]],
                          amw_analysis_data_path = PFUSetup::get_abs_paths(version = version)[["amw_analysis_data_path"]],
                          hmw_analysis_data_path = PFUSetup::get_abs_paths(version = version)[["hmw_analysis_data_path"]],
                          phi_constants_path = PFUSetup::get_abs_paths(version = version)[["phi_constants_path"]],
                          # Temperature data not required for V1, argument set to NULL.
                          ceda_data_folder = NULL,
                          fao_data_path = PFUSetup::get_abs_paths(version = version)[["fao_data_path"]],
                          ilo_data_path = PFUSetup::get_abs_paths(version = version)[["ilo_data_path"]],
                          machine_data_path = PFUSetup::get_abs_paths(version = version)[["machine_data_folder"]],
                          exemplar_table_path = PFUSetup::get_abs_paths(version = version)[["exemplar_table_path"]],
                          fu_analysis_folder = PFUSetup::get_abs_paths(version = version)[["fu_analysis_folder"]],
                          reports_source_folders = PFUSetup::get_abs_paths(version = version)[["reports_source_folders"]],
                          reports_dest_folder = PFUSetup::get_abs_paths(version = version)[["reports_dest_folder"]],
                          pipeline_releases_folder = PFUSetup::get_abs_paths(version = version)[["pipeline_releases_folder"]],
                          pipeline_caches_folder = PFUSetup::get_abs_paths(version = version)[["pipeline_caches_folder"]],
                          release = release)

