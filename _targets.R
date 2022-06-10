library(magrittr)
library(targets)
library(PFUDatabase)
# targets::tar_make() to run the pipeline
# targets::tar_make_future(workers = 8) to execute across multiple cores.
# targets::tar_read(<<target_name>>) to view the results.
# targets::tar_destroy() to start over with everything.
# targets::tar_make(callr_function = NULL) to debug.


# Custom parameters
# years <- 1960:2019                         # The years to be analyzed
years <- 1971

# countries <- PFUDatabase::canonical_countries %>% as.character()

# Countries with unique allocations data.
# countries <- c("BRA", "CAN", "CHNM", "DEU", "DNK", "ESP", "FRA", "GBR", "GHA",
#                "GRC", "HKG", "HND", "IDN", "IND", "JOR", "JPN", "KOR", "MEX",
#                "NOR", "PRT", "RUS", "USA", "WABK", "WMBK", "ZAF")

# countries <- c("GBR")
# countries <- c("SUN")


# Trying to find which country(ies) are causing the failures.

# This fails:
# countries <- c("BRA", "CAN", "CHNM", "DEU", "DNK", "ESP", "FRA", "GBR", "GHA",
#                "GRC", "HKG", "HND", "IDN", "IND", "JOR", "JPN", "KOR", "MEX",
#                "NOR", "PRT", "RUS", "USA", "WABK", "WMBK", "ZAF")

# Try first half of countries.  
# This works.
# countries <- c("BRA", "CAN", "CHNM", "DEU", "DNK", "ESP", "FRA", "GBR", "GHA",
#                "GRC", "HKG", "HND", "IDN")

# Fails
# countries <- c("IND", "JOR", "JPN", "KOR", "MEX",
#                "NOR", "PRT", "RUS", "USA", "WABK", "WMBK", "ZAF")

# Works
# countries <- c("IND", "JOR", "JPN", "KOR", "MEX", "NOR")

# Fails
# countries <- c("PRT", "RUS", "USA", "WABK", "WMBK", "ZAF")

# Fails
# countries <- c("PRT", "RUS", "USA")


countries <- c("RUS")


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
                          mw_concordance_path = PFUSetup::get_abs_paths()[["mw_concordance_path"]],
                          amw_analysis_data_path = PFUSetup::get_abs_paths()[["amw_analysis_data_path"]],
                          hmw_analysis_data_path = PFUSetup::get_abs_paths()[["hmw_analysis_data_path"]],
                          phi_constants_path = PFUSetup::get_abs_paths()[["phi_constants_path"]],
                          # Temperature data not required for V1, argument set to NULL.
                          ceda_data_folder = NULL,
                          fao_data_path = PFUSetup::get_abs_paths()[["fao_data_path"]],
                          ilo_data_path = PFUSetup::get_abs_paths()[["ilo_data_path"]],
                          machine_data_path = PFUSetup::get_abs_paths()[["machine_data_folder"]],
                          exemplar_table_path = PFUSetup::get_abs_paths()[["exemplar_table_path"]],
                          fu_analysis_folder = PFUSetup::get_abs_paths()[["fu_analysis_folder"]],
                          reports_source_folders = PFUSetup::get_abs_paths()[["reports_source_folders"]],
                          reports_dest_folder = PFUSetup::get_abs_paths()[["reports_dest_folder"]],
                          pipeline_releases_folder = PFUSetup::get_abs_paths()[["pipeline_releases_folder"]],
                          release = release)

