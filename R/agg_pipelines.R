#' Create an analysis pipeline
#' 
#' The pipeline a `targets` pipeline that creates the PFU database.
#'
#' @param countries A vector of abbreviations for countries whose energy conversion chain is to be analyzed,
#'                        such as "c('GHA', 'ZAF')".
#'                        Countries named in `which_countries` can also serve as exemplars for
#'                        final-to-useful allocations and efficiencies.
#' @param additional_exemplar_countries A vector of country abbreviations for which final-to-useful allocations
#'                                      and efficiencies will be read.
#'                                      An energy conversion chain will _not_ be constructed for these countries.
#'                                      However, their final-to-useful allocations and efficiencies
#'                                      may be used as exemplar information for the countries in `countries`.
#'                                      Default is `NULL`, indicating no additional exemplars.
#' @param years The years to be studied.
#' @param how_far A string indicating the last target to include in the plan that is returned.
#'                Default is "all_targets" to indicate all targets of the plan should be returned.
#' @param iea_data_path The path to IEA extended energy balance data in .csv format.
#' @param country_concordance_path The path to the country concordance Excel file.
#' @param phi_constants_path The path to a phi (exergy-to-energy ratio) Excel file.
#' @param ceda_data_folder The path to the CEDA data in text file, .per, format.
#' @param machine_data_path The path to the machine data in .xlsx format.
#' @param exemplar_table_path The path to an exemplar table.
#' @param fu_analysis_folder The path to a folder containing final-to-useful analyses.
#'                           Sub-folders named with 3-letter country abbreviations are assumed.
#' @param reports_source_folders A string vector containing paths to folders of report sources, usually
#'                               `.Rnw` or `.Rmd` files.
#' @param reports_dest_folder The path to a folder into which reports are written.
#' @param pipeline_caches_folder The path to a folder where .zip files of the pipeline cache folders are stored.
#' @param pipeline_releases_folder The path to a folder where releases of important targets are stored
#'                                 for later retrieval as pinned items on a pinboard.
#' @param release A boolean that tells whether a new release of the `PSUT` target should be made.
#'                Default is `FALSE`.
#'
#' @return A `targets` pipeline.
#' 
#' @export
get_pipeline <- function(countries = "all",
                         additional_exemplar_countries = NULL,
                         years = "all",
                         how_far = "all_targets",
                         iea_data_path,
                         country_concordance_path,
                         phi_constants_path,
                         ceda_data_folder,
                         machine_data_path,
                         exemplar_table_path,
                         fu_analysis_folder,
                         reports_source_folders,
                         reports_dest_folder,
                         pipeline_caches_folder,
                         pipeline_releases_folder,
                         release = FALSE) {
  
  # Eliminate warnings in R CMD CHECK
  Country <- NULL
  FilteredAllIEAData <- NULL
  IEAData <- NULL
  
  # Create the pipeline
  list(

    # (0) Set many arguments to be objects in the targets cache for later use
    targets::tar_target_raw("Countries", rlang::enexpr(countries)),
    targets::tar_target_raw("AdditionalExemplarCountries", rlang::enexpr(additional_exemplar_countries)), 
    targets::tar_target_raw("AllocAndEffCountries", quote(combine_countries_exemplars(Countries, AdditionalExemplarCountries))),
    targets::tar_target_raw("Years", rlang::enexpr(years)), 
    targets::tar_target_raw("IEADataPath", iea_data_path), 
    targets::tar_target_raw("CountryConcordancePath", country_concordance_path), 
    targets::tar_target_raw("PhiConstantsPath", phi_constants_path), 
    targets::tar_target_raw("CEDADataFolder", ceda_data_folder), 
    targets::tar_target_raw("MachineDataPath", machine_data_path), 
    targets::tar_target_raw("ExemplarTablePath", exemplar_table_path), 
    targets::tar_target_raw("FUAnalysisFolder", fu_analysis_folder), 
    targets::tar_target_raw("ReportsSourceFolders", reports_source_folders), 
    targets::tar_target_raw("ReportsDestFolder", reports_dest_folder), 
    targets::tar_target_raw("PipelineCachesFolder", pipeline_caches_folder), 
    targets::tar_target_raw("PipelineReleasesFolder", pipeline_releases_folder), 
    targets::tar_target_raw("Release", release), 
    
    
    # (1) Load pipeline information
    
    # (1a) Country concordance table
    targets::tar_target_raw("CountryConcordanceTable", quote(load_country_concordance_table(country_concordance_path = CountryConcordancePath))),
    
    # (1b) Final demand sectors
    targets::tar_target_raw("FinalDemandSectors", quote(get_fd_sectors())), 
    
    # (1c) Primary industry prefixes
    targets::tar_target_raw("PrimaryIndustryPrefixes", quote(get_p_industry_prefixes())),
    
    # (1d) IEA data
    targets::tar_target_raw("AllIEAData", quote(IEATools::load_tidy_iea_df(IEADataPath, override_df = CountryConcordanceTable))),
    targets::tar_target_raw("FilteredAllIEAData", quote(filter_countries_years(AllIEAData, countries = AllocAndEffCountries, years = Years))),
    tarchetypes::tar_group_by(IEAData, command = FilteredAllIEAData, Country), 
    
    # (1e) CEDA data for ALL countries
    targets::tar_target_raw("CEDAData", quote(CEDATools::create_agg_cru_cy_df(agg_cru_cy_folder = CEDADataFolder,
                                                                     agg_cru_cy_metric = c("tmp", "tmn", "tmx"),
                                                                     agg_cru_cy_year = 2020))), 
    
    # (1f) Machine data 
    targets::tar_target_raw("AllMachineData", quote(read_all_eta_files(eta_fin_paths = get_eta_filepaths(MachineDataPath)))), 
    targets::tar_target_raw("MachineData", quote(filter_countries_years(AllMachineData, countries = AllocAndEffCountries, years = Years))),
    
    # (1g) Socioeconomic data
    targets::tar_target_raw("SocioEconData", quote(get_all_pwt_data(countries = Countries) %>% get_L_K_GDP_data())) # , 

    
    # (2) Balance all final energy data.
    # First, check whether energy products are balanced. They're not.
    # FALSE indicates a country with at least one balance problem.
    # 
    # Not working yet. function "map" is unknown.
    # targets::tar_target_raw("BalancedBefore", quote(is_balanced(IEAData, countries = AllocAndEffCountries)),
    #                pattern = map(IEAData), storage = "worker", retrieval = "worker")
    
    
    )
  
  
  
}