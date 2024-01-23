library(magrittr)
library(targets)
library(PFUPipeline)
# targets::tar_make() to run the pipeline
# targets::tar_make_future(workers = 8) to execute across multiple cores.
# targets::tar_read(<<target_name>>) to view the results.
# targets::tar_destroy() to start over with everything.
# targets::tar_make(callr_function = NULL) to debug.


# Input data version
version <- "v1.3"

# Custom parameters

# The years to be analyzed

# years <- 1971:2020                        
# years <- 1971
# years <- 1971:1972
# years <- 1971:1980
# years <- 1995
# years <- 1995:1996
# years <- 1995:2020
years <- 1995:2020
# years <- 1960:2020

# Set the years to provide exiobase coefficients
# years_exiobase <- 1995:1996
years_exiobase <- 1995:2020

# countries <- c("ARM", "COL", "WRLD")
# countries <- c("GBR", "MEX", "GUY")
countries <- "USA"
# countries <- "GHA"
# countries <- "COL"
# countries <- "AGO"
# countries <- "WMBK"
# countries <- c("AGO", "COL")
# countries <- "WRLD"
# countries <- c("BEN", "WRLD")
# countries <- c(PFUPipeline::canonical_countries, "WRLD") |> as.character()

# Countries with unique allocation data.
# countries <- c("WRLD", "BRA", "CAN", "CHNM", "DEU", "DNK", "ESP", "FRA", "GBR", "GHA",
#                "GRC", "HKG", "HND", "IDN", "IND", "JOR", "JPN", "KOR", "MEX", "NOR",
#                "PRT", "RUS", "USA", "WABK", "WMBK", "ZAF")

# Additional exemplar countries are countries which aren't included in the workflow
# as individual countries, but from which allocation or efficiency data may be 
# obtained and assigned to countries in the workflow using the exemplar system.
additional_exemplar_countries <- c("AFRI", # Africa 
                                   "ASIA", # Asia
                                   "EURP", # Europe 
                                   "MIDE", # Middle East
                                   "NAMR", # North America
                                   "OCEN", # Oceania 
                                   "SAMR", # South America 
                                   "BUNK") # Bunkers

# Which type of matrix objects should be created?
# "matrix" is the built-in matrix object in R.
# "Matrix" will provide sparse matrices.
# matrix_class <- "matrix"
matrix_class <- "Matrix"

# Should we specify non-energy flows?
specify_non_energy_flows <- TRUE

# Should we apply fixes to the IEA data?
apply_fixes <- TRUE

# Should we do a release of the results?
release <- TRUE

# End user-adjustable parameters.

# WRLD should not be in both countries and additional_exemplar_countries
if (("WRLD" %in% countries) & ("WRLD" %in% additional_exemplar_countries)) {
  # Remove WRLD from additional_exemplar_countries
  additional_exemplar_countries <- additional_exemplar_countries[!(additional_exemplar_countries == "WRLD")]
}

# WRLD should always be in countries or in additional_exemplar_countries.
if (!("WRLD" %in% countries) & !("WRLD" %in% additional_exemplar_countries)) {
  # Add WRLD to additional_exemplar_countries
  additional_exemplar_countries <- c("WRLD", additional_exemplar_countries)
}


# Getting default filepaths
sys_info <- Sys.info()
setup <- PFUSetup::get_abs_paths(version = version)

# Amending for EA
if ((sys_info[["sysname"]] == "Linux") && (sys_info[["user"]] == "eeear")){
  setup[["iea_data_path"]] <- "/home/eeear/Documents/Datasets/IEA/WEEBs/IEA Extended Energy Balances 2022 (TJ).csv"
  setup[["aggregation_mapping_path"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/aggregation_mapping.xlsx"
  setup[["country_concordance_path"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/Country_Concordance_Full.xlsx"
  setup[["mw_concordance_path"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/FAO_ISO_MW_Mapping.xlsx"
  setup[["amw_analysis_data_path"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/amw_analysis_data.xlsx"
  setup[["hmw_analysis_data_path"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/hmw_analysis_data.xlsx"
  setup[["phi_constants_path"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/phi_constants.xlsx"
  setup[["fao_data_path"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/fao_qcl_data.rds"
  setup[["ilo_employment_data_path"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/ilo_employment_data.rds"
  setup[["ilo_working_hours_data_path"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/ilo_working_hours_data.rds"
  setup[["machine_data_folder"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/Machines - Data"
  setup[["exemplar_table_path"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/Exemplar_Table.xlsx"
  setup[["fu_analysis_folder"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/FU analysis data"#
  setup[["exiobase_energy_flows_path"]] <- "/home/eeear/Documents/Datasets/GPFU_database/InputData/v1.2/exiobase_energy_flows_concordance.xlsx"
  #setup[["reports_source_folders"]]
  setup[["reports_dest_folder"]] <- "/home/eeear/Documents/Datasets/GPFU_database/OutputData/Reports"
  setup["pipeline_releases_folder"] <- "/home/eeear/Documents/Datasets/GPFU_database/OutputData/Releases"
  setup[["pipeline_caches_folder"]] <- "/home/eeear/Documents/Datasets/GPFU_database/OutputData/PipelineCaches"
}



# Set up for multithreaded work on the local machine.
future::plan(future.callr::callr)

# Set options for all targets.
targets::tar_option_set(
  storage = "worker", 
  retrieval = "worker"
)

# Pull in the pipeline
PFUPipeline::get_pipeline(countries = countries,
                          additional_exemplar_countries = additional_exemplar_countries,
                          matrix_class = matrix_class,
                          specify_non_energy_flows = specify_non_energy_flows,
                          apply_fixes = apply_fixes,
                          years = years,
                          how_far = "all_targets",
                          iea_data_path = setup[["iea_data_path"]],
                          country_concordance_path = setup[["country_concordance_path"]],
                          mw_concordance_path = setup[["mw_concordance_path"]],
                          amw_analysis_data_path = setup[["amw_analysis_data_path"]],
                          hmw_analysis_data_path = setup[["hmw_analysis_data_path"]],
                          phi_constants_path = setup[["phi_constants_path"]],
                          # Temperature data not required for V1, argument set to NULL.
                          ceda_data_folder = NULL,
                          fao_data_path = setup[["fao_data_path"]],
                          ilo_employment_data_path = setup[["ilo_employment_data_path"]],
                          ilo_working_hours_data_path = setup[["ilo_working_hours_data_path"]],
                          machine_data_path = setup[["machine_data_folder"]],
                          exemplar_table_path = setup[["exemplar_table_path"]],
                          fu_analysis_folder = setup[["fu_analysis_folder"]],
                          exiobase_energy_flows_path = setup[["exiobase_energy_flows_path"]],
                          years_exiobase = years_exiobase,
                          reports_source_folders = setup[["reports_source_folders"]],
                          reports_dest_folder = setup[["reports_dest_folder"]],
                          pipeline_releases_folder = setup[["pipeline_releases_folder"]],
                          pipeline_caches_folder = setup[["pipeline_caches_folder"]],
                          release = release)

