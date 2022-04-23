#' Create an analysis pipeline
#' 
#' The pipeline a `targets` pipeline that creates the PFU database.
#'
#' @param countries A vector of abbreviations for countries whose energy conversion chain is to be analyzed,
#'                  such as "c('GHA', 'ZAF')".
#'                  Countries named in `countries` can also serve as exemplars for
#'                  final-to-useful allocations and efficiencies.
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
  
  # Create the pipeline
  list(
    
    # (0) Set many arguments to be objects in the targets cache for later use
    targets::tar_target_raw("Countries", list(countries)),
    targets::tar_target_raw("AdditionalExemplarCountries", list(additional_exemplar_countries)), 
    targets::tar_target_raw("AllocAndEffCountries", quote(combine_countries_exemplars(Countries, AdditionalExemplarCountries))),
    targets::tar_target_raw("Years", list(years)), 
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
    # targets::tar_target_raw("AllIEAData", quote(IEATools::load_tidy_iea_df(IEADataPath, override_df = CountryConcordanceTable)), 
    #                         storage = "worker", retrieval = "worker"),
    # targets::tar_target_raw("FilteredAllIEAData", quote(filter_countries_years(AllIEAData, countries = AllocAndEffCountries, years = Years))),
    # tarchetypes::tar_group_by(IEAData, command = FilteredAllIEAData, Country), 
    
    # targets::tar_target_raw("FilteredIEAData", quote(IEATools::load_tidy_iea_df(IEADataPath, override_df = CountryConcordanceTable) %>% 
    #                             filter_countries_years(countries = AllocAndEffCountries, years = Years))), 
    # tarchetypes::tar_group_by(IEAData, FilteredIEAData, Country),

    # tarchetypes::tar_group_by(IEAData, IEATools::load_tidy_iea_df(IEADataPath, override_df = CountryConcordanceTable) %>% 
    #                             filter_countries_years(countries = AllocAndEffCountries, years = Years), 
    #                           Country), 
    targets::tar_target_raw("IEAData", quote(IEATools::load_tidy_iea_df(IEADataPath, override_df = CountryConcordanceTable) %>% 
                                filter_countries_years(countries = AllocAndEffCountries, years = Years)),
                            storage = "worker", retrieval = "worker"), 
    
    # (1e) CEDA data for ALL countries
    targets::tar_target_raw("CEDAData", quote(CEDATools::create_agg_cru_cy_df(agg_cru_cy_folder = CEDADataFolder,
                                                                              agg_cru_cy_metric = c("tmp", "tmn", "tmx"),
                                                                              agg_cru_cy_year = 2020)), 
                            storage = "worker", retrieval = "worker"), 
    
    # (1f) Machine data 
    targets::tar_target_raw("AllMachineData", quote(read_all_eta_files(eta_fin_paths = get_eta_filepaths(MachineDataPath)))),
    targets::tar_target_raw("MachineData", quote(filter_countries_years(AllMachineData, countries = AllocAndEffCountries, years = Years)),
                            storage = "worker", retrieval = "worker"),
    
    # (1g) Socioeconomic data
    targets::tar_target_raw("SocioEconData", quote(get_all_pwt_data(countries = Countries) %>% get_L_K_GDP_data())), 
    
    # (2) Balance all final energy data.
    # First, check whether energy products are balanced. They're not.
    # FALSE indicates a country with at least one balance problem.
    targets::tar_target_raw("BalancedBefore", quote(is_balanced(IEAData, countries = AllocAndEffCountries)),
                            pattern = quote(map(AllocAndEffCountries)),
                            storage = "worker", retrieval = "worker"), 
    
    # Balance all of the data by product and year.
    targets::tar_target_raw("BalancedIEAData", quote(make_balanced(IEAData, countries = AllocAndEffCountries)), 
                            pattern = quote(map(AllocAndEffCountries)),
                            storage = "worker", retrieval = "worker"),
    
    # Check that balancing was successful.
    targets::tar_target_raw("BalancedAfter", quote(is_balanced(BalancedIEAData, countries = AllocAndEffCountries)), 
                            pattern = quote(map(AllocAndEffCountries)),
                            storage = "worker", retrieval = "worker"), 
    
    # Don't continue if there is a problem.
    # stopifnot returns NULL if everything is OK.
    targets::tar_target_raw("OKToProceed", quote(ifelse(is.null(stopifnot(all(BalancedAfter))), yes = TRUE, no = FALSE))),
    
    # (3) Specify the BalancedIEAData data frame by being more careful with names, etc.
    targets::tar_target_raw("Specified", quote(specify(BalancedIEAData, countries = AllocAndEffCountries)) ,
                            pattern = quote(map(AllocAndEffCountries)),
                            storage = "worker", retrieval = "worker"),
    
    # (4) Arrange all the data into PSUT matrices with final stage data.
    targets::tar_target_raw("PSUTFinal", quote(make_psut(Specified, countries = Countries)), 
                            pattern = quote(map(Countries)), 
                            storage = "worker", retrieval = "worker"),
    
    # (5) Load exemplar table and make lists for each country and year from disk.
    # These may be incomplete.
    targets::tar_target_raw("ExemplarLists", quote(load_exemplar_table(ExemplarTablePath, 
                                                                       countries = AllocAndEffCountries,
                                                                       years = Years) %>%
                                                     exemplar_lists(AllocAndEffCountries)), 
                            pattern = quote(map(AllocAndEffCountries)),
                            storage = "worker", retrieval = "worker"),
    
    # (6) Load phi (exergy-to-energy ratio) constants
    targets::tar_target_raw("PhiConstants", quote(IEATools::load_phi_constants_table(PhiConstantsPath)), 
                            storage = "worker", retrieval = "worker"), 
    
    # (7) Load incomplete FU allocation tables
    targets::tar_target_raw("IncompleteAllocationTables", quote(load_fu_allocation_tables(FUAnalysisFolder,
                                                                                          specified_iea_data = Specified,
                                                                                          countries = AllocAndEffCountries)),
                            pattern = quote(map(AllocAndEffCountries)),
                            storage = "worker", retrieval = "worker"),

    # The next target is never used. So no need to calculate it.  
    # Delete after 22 May 2022. ---MKH, 22 April 2022
    # tarchetypes::tar_group_by(TidyIncompleteAllocationTables, 
    #                           IEATools::tidy_fu_allocation_table(IncompleteAllocationTables), 
    #                           Country), 
    
    # (8) Complete FU allocation tables
    targets::tar_target_raw("CompletedAllocationTables", quote(assemble_fu_allocation_tables(incomplete_allocation_tables = IncompleteAllocationTables,
                                                                                             exemplar_lists = ExemplarLists,
                                                                                             specified_iea_data = Specified %>% dplyr::mutate(tar_group = NULL),
                                                                                             countries = Countries,
                                                                                             years = Years)), 
                            pattern = quote(map(Countries)), 
                            storage = "worker", retrieval = "worker"),

    # (9) Complete efficiency tables
    targets::tar_target_raw("CompletedEfficiencyTables", quote(assemble_eta_fu_tables(incomplete_eta_fu_tables = MachineData,
                                                                                      exemplar_lists = ExemplarLists,
                                                                                      completed_fu_allocation_tables = CompletedAllocationTables,
                                                                                      countries = Countries,
                                                                                      years = Years,
                                                                                      which_quantity = IEATools::template_cols$eta_fu)), 
                            pattern = quote(map(Countries)), 
                            storage = "worker", retrieval = "worker"), 
    
    # (10) Complete phi_u tables
    targets::tar_target_raw("CompletedPhiuTables", quote(assemble_phi_u_tables(incomplete_phi_u_table = MachineData,
                                                                               phi_constants_table = PhiConstants,
                                                                               completed_efficiency_table = CompletedEfficiencyTables,
                                                                               countries = Countries,
                                                                               years = Years)), 
                            pattern = quote(map(Countries)), 
                            storage = "worker", retrieval = "worker"), 
    
    # (11) Build matrices and vectors for extending to useful stage and exergy
    targets::tar_target_raw("Cmats", quote(calc_C_mats(completed_allocation_tables = CompletedAllocationTables,
                                                       countries = Countries)), 
                            pattern = quote(map(Countries)), 
                            storage = "worker", retrieval = "worker"), 

    targets::tar_target_raw("EtafuPhiuvecs", quote(calc_eta_fu_phi_u_vecs(completed_efficiency_tables = CompletedEfficiencyTables,
                                                                          completed_phi_tables = CompletedPhiuTables,
                                                                          countries = Countries)), 
                            pattern = quote(map(Countries)), 
                            storage = "worker", retrieval = "worker"), 
    
    targets::tar_target_raw("Etafuvecs", quote(sep_eta_fu_phi_u(EtafuPhiuvecs,
                                                                keep = IEATools::template_cols$eta_fu,
                                                                countries = Countries)), 
                            pattern = quote(map(Countries)), 
                            storage = "worker", retrieval = "worker"), 
    
    targets::tar_target_raw("Phiuvecs", quote(sep_eta_fu_phi_u(EtafuPhiuvecs,
                                                               keep = IEATools::template_cols$phi_u,
                                                               countries = Countries)), 
                            pattern = quote(map(Countries)), 
                            storage = "worker", retrieval = "worker"), 
    
    targets::tar_target_raw("Phipfvecs", quote(calc_phi_pf_vecs(phi_u_vecs = Phiuvecs,
                                                                phi_constants = PhiConstants,
                                                                countries = Countries)), 
                            pattern = quote(map(Countries)), 
                            storage = "worker", retrieval = "worker"), 
    
    targets::tar_target_raw("Phivecs", quote(sum_phi_vecs(phi_pf_vecs = Phipfvecs,
                                                          phi_u_vecs = Phiuvecs,
                                                          countries = Countries)), 
                            pattern = quote(map(Countries)), 
                            storage = "worker", retrieval = "worker")
    
    




    
    
    
    
  )
  
  
  
}