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
#' @param matrix_class One of "matrix" or "Matrix", specifying which kind of matrix 
#'                     objects are to be created.
#'                     Default is "matrix".
#' @param specify_non_energy_flows A boolean that tells whether to provide additional
#'                                 specificity to non-energy flows, when available.
#'                                 Default is `FALSE`.
#' @param apply_fixes A boolean that tells whether to fix some of the IEA WEEB data, 
#'                    where possible.
#'                    Default is `FALSE`.
#' @param years The years to be studied.
#' @param how_far A string indicating the last target to include in the plan that is returned.
#'                Default is "all_targets" to indicate all targets of the plan should be returned.
#' @param iea_data_path The path to IEA extended energy balance data in .csv format.
#' @param country_concordance_path The path to the country concordance Excel file.
#' @param mw_concordance_path The path to the muscle work concordance file.
#' @param amw_analysis_data_path The path to the animal muscle work data file.
#' @param hmw_analysis_data_path The path to the human muscle work data file.
#' @param phi_constants_path The path to a phi (exergy-to-energy ratio) Excel file.
#' @param ceda_data_folder The path to the CEDA data in text file, .per, format.
#' @param fao_data_path The path to Food and Agriculture Organization (FAO) data.
#' @param ilo_data_path The path to International Labor Organization (ILO) data.
#' @param machine_data_path The path to the machine data in .xlsx format.
#' @param exemplar_table_path The path to an exemplar table.
#' @param fu_analysis_folder The path to a folder containing final-to-useful analyses.
#'                           Sub-folders named with 3-letter country abbreviations are assumed.
#' @param reports_source_folders A string vector containing paths to folders of report sources, usually
#'                               `.Rnw` or `.Rmd` files.
#' @param reports_dest_folder The path to a folder into which reports are written.
#' @param pipeline_releases_folder The path to a folder where releases of important targets are stored
#'                                 for later retrieval as pinned items on a pinboard.
#' @param pipeline_caches_folder The path to a folder where .zipped versions of the pipeline
#'                               cache are stored.
#' @param release A boolean that tells whether a new release of the `PSUT` targets should be made.
#'                Default is `FALSE`.
#'
#' @return A `targets` pipeline.
#' 
#' @export
get_pipeline <- function(countries = "all",
                         additional_exemplar_countries = NULL,
                         specify_non_energy_flows = FALSE,
                         matrix_class = c("matrix", "Matrix"), 
                         apply_fixes = FALSE, 
                         years = "all",
                         how_far = "all_targets",
                         iea_data_path,
                         country_concordance_path,
                         mw_concordance_path,
                         amw_analysis_data_path, 
                         hmw_analysis_data_path,
                         phi_constants_path,
                         ceda_data_folder,
                         fao_data_path,
                         ilo_data_path,
                         machine_data_path,
                         exemplar_table_path,
                         fu_analysis_folder,
                         reports_source_folders,
                         reports_dest_folder,
                         pipeline_releases_folder,
                         pipeline_caches_folder,
                         release = FALSE) {
  
  matrix_class <- match.arg(matrix_class)
  
  # Create the pipeline
  list(

    # (0) Set many arguments to be objects in the targets cache for later use
    targets::tar_target_raw("Countries", list(countries)),
    targets::tar_target_raw("AdditionalExemplarCountries", list(additional_exemplar_countries)),
    targets::tar_target_raw("SpecifyNonEnergyFlows", list(specify_non_energy_flows)),
    targets::tar_target_raw("ApplyFixes", list(apply_fixes)),
    targets::tar_target_raw("AllocAndEffCountries", quote(combine_countries_exemplars(Countries, AdditionalExemplarCountries))),
    targets::tar_target_raw("Years", list(years)),
    targets::tar_target_raw("IEADataPath", iea_data_path),
    targets::tar_target_raw("CountryConcordancePath", country_concordance_path),
    targets::tar_target_raw("MWConcordancePath", mw_concordance_path),
    targets::tar_target_raw("AMWAnalysisDataPath", amw_analysis_data_path),
    targets::tar_target_raw("HMWAnalysisDataPath", hmw_analysis_data_path),
    targets::tar_target_raw("PhiConstantsPath", phi_constants_path),
    # Temperature data not required for V1
    # targets::tar_target_raw("CEDADataFolder", ceda_data_folder),
    targets::tar_target_raw("FAODataPath", fao_data_path),
    targets::tar_target_raw("ILODataPath", ilo_data_path),
    targets::tar_target_raw("MachineDataPath", machine_data_path),
    targets::tar_target_raw("ExemplarTablePath", exemplar_table_path),
    targets::tar_target_raw("FUAnalysisFolder", fu_analysis_folder),
    targets::tar_target_raw("ReportsSourceFolders", reports_source_folders),
    targets::tar_target_raw("ReportsDestFolder", reports_dest_folder),
    targets::tar_target_raw("PipelineReleasesFolder", pipeline_releases_folder),
    targets::tar_target_raw("PipelineCachesFolder", pipeline_caches_folder),
    targets::tar_target_raw("Release", release),
    
    
    # (1) Load pipeline information

    # (1a) IEA data
    targets::tar_target_raw("AllIEAData", quote(IEATools::load_tidy_iea_df(IEADataPath, 
                                                                           override_df = CountryConcordanceTable, 
                                                                           specify_non_energy_flows = SpecifyNonEnergyFlows, 
                                                                           apply_fixes = ApplyFixes))),
    targets::tar_target_raw("IEAData", quote(AllIEAData %>%
                                               filter_countries_years(countries = AllocAndEffCountries, years = Years))),

    # (1b) Country concordance table
    targets::tar_target_raw("CountryConcordanceTable", quote(load_country_concordance_table(country_concordance_path = CountryConcordancePath))),

    # Temperature data not required for V1
    # (1c) CEDA data for ALL countries
    # targets::tar_target_raw("CEDAData", quote(CEDATools::create_agg_cru_cy_df(agg_cru_cy_folder = CEDADataFolder,
    #                                                                           agg_cru_cy_metric = c("tmp", "tmn", "tmx"),
    #                                                                           agg_cru_cy_year = 2020))),

    # (1d) Machine data
    targets::tar_target_raw("AllMachineData", quote(read_all_eta_files(eta_fin_paths = get_eta_filepaths(MachineDataPath)))),
    targets::tar_target_raw("MachineData", quote(filter_countries_years(AllMachineData, countries = AllocAndEffCountries, years = Years))),

    # (1e) Muscle work data
    targets::tar_target_raw("AMWPFUDataRaw", quote(load_amw_pfu_data(fao_data_path = FAODataPath,
                                                                     mw_concordance_path = MWConcordancePath,
                                                                     amw_analysis_data_path = AMWAnalysisDataPath))),

    targets::tar_target_raw("HMWPFUDataRaw", quote(load_hmw_pfu_data(ilo_data_path = ILODataPath,
                                                                     mw_concordance_path = MWConcordancePath,
                                                                     hmw_analysis_data_path = HMWAnalysisDataPath))),

    targets::tar_target_raw("AMWPFUData", quote(aggcountries_mw_to_iea(mw_df = AMWPFUDataRaw))),

    targets::tar_target_raw("HMWPFUData", quote(aggcountries_mw_to_iea(mw_df = HMWPFUDataRaw))),

    # Socio-economic data not required for V1
    # (1f) Socioeconomic data
    # targets::tar_target_raw("SocioEconData", quote(get_all_pwt_data(countries = Countries) %>% get_L_K_GDP_data())),

    # (2) Balance all IEA final energy data.
    # First, check whether energy products are balanced. They're not.
    # FALSE indicates a country with at least one balance problem.
    targets::tar_target_raw("BalancedBeforeIEA", quote(is_balanced(IEAData, countries = AllocAndEffCountries)),
                            pattern = quote(map(AllocAndEffCountries))),

    # Balance all of the data by product and year.
    targets::tar_target_raw("BalancedIEAData", quote(make_balanced(IEAData, countries = AllocAndEffCountries)),
                            pattern = quote(map(AllocAndEffCountries))),

    # Check that balancing was successful.
    targets::tar_target_raw("BalancedAfterIEA", quote(is_balanced(BalancedIEAData, countries = AllocAndEffCountries)),
                            pattern = quote(map(AllocAndEffCountries))),

    # Don't continue if there is a problem.
    # stopifnot returns NULL if everything is OK.
    targets::tar_target_raw("OKToProceedIEA", quote(ifelse(is.null(stopifnot(all(BalancedAfterIEA))), yes = TRUE, no = FALSE))),

    # (3) Specify the BalancedIEAData data frame by being more careful with names, etc.
    targets::tar_target_raw("SpecifiedIEA", quote(specify(BalancedIEAData, countries = AllocAndEffCountries)) ,
                            pattern = quote(map(AllocAndEffCountries))),

    # (4) Arrange all the data into PSUT matrices with final stage data.
    targets::tar_target_raw("PSUTFinalIEA", quote(make_iea_psut(SpecifiedIEA, countries = Countries, matrix_class = matrix_class)),
                            pattern = quote(map(Countries))),

    # (5) Load exemplar table and make lists for each country and year from disk.
    # These may be incomplete.
    targets::tar_target_raw("ExemplarLists", quote(load_exemplar_table(ExemplarTablePath,
                                                                       countries = AllocAndEffCountries,
                                                                       years = Years) %>%
                                                     exemplar_lists(AllocAndEffCountries)),
                            pattern = quote(map(AllocAndEffCountries))),

    # (6) Load phi (exergy-to-energy ratio) constants
    targets::tar_target_raw("PhiConstants", quote(IEATools::load_phi_constants_table(PhiConstantsPath))),

    # (7) Load incomplete FU allocation tables
    targets::tar_target_raw("IncompleteAllocationTables", quote(load_fu_allocation_tables(FUAnalysisFolder,
                                                                                          specified_iea_data = SpecifiedIEA,
                                                                                          countries = AllocAndEffCountries)),
                            pattern = quote(map(AllocAndEffCountries))),

    # (8) Complete FU allocation tables
    targets::tar_target_raw("CompletedAllocationTables", quote(assemble_fu_allocation_tables(incomplete_allocation_tables = IncompleteAllocationTables,
                                                                                             exemplar_lists = ExemplarLists,
                                                                                             specified_iea_data = SpecifiedIEA %>% dplyr::mutate(tar_group = NULL),
                                                                                             countries = Countries,
                                                                                             years = Years)),
                            pattern = quote(map(Countries))),

    # (9) Complete efficiency tables
    targets::tar_target_raw("CompletedEfficiencyTables", quote(assemble_eta_fu_tables(incomplete_eta_fu_tables = MachineData,
                                                                                      exemplar_lists = ExemplarLists,
                                                                                      completed_fu_allocation_tables = CompletedAllocationTables,
                                                                                      countries = Countries,
                                                                                      years = Years,
                                                                                      which_quantity = IEATools::template_cols$eta_fu)),
                            pattern = quote(map(Countries))),

    # (10) Complete phi_u tables
    targets::tar_target_raw("CompletedPhiuTables", quote(assemble_phi_u_tables(incomplete_phi_u_table = MachineData,
                                                                               phi_constants_table = PhiConstants,
                                                                               completed_efficiency_table = CompletedEfficiencyTables,
                                                                               countries = Countries,
                                                                               years = Years)),
                            pattern = quote(map(Countries))),

    # (11) Build matrices and vectors for extending to useful stage and exergy
    # (11a) Allocation (C) matrices
    targets::tar_target_raw("Cmats", quote(calc_C_mats(completed_allocation_tables = CompletedAllocationTables,
                                                       countries = Countries, 
                                                       matrix_class = matrix_class)),
                            pattern = quote(map(Countries))),

    # (11b) Final-to-useful efficiency (eta_fu) and exergy-to-energy ratio (phi_u) vectors at the useful stage
    targets::tar_target_raw("EtafuPhiuvecs", quote(calc_eta_fu_phi_u_vecs(completed_efficiency_tables = CompletedEfficiencyTables,
                                                                          completed_phi_tables = CompletedPhiuTables,
                                                                          countries = Countries, 
                                                                          matrix_class = matrix_class)),
                            pattern = quote(map(Countries))),

    # (11c) Final-to-useful efficiency (eta_fu) vectors
    targets::tar_target_raw("Etafuvecs", quote(sep_eta_fu_phi_u(EtafuPhiuvecs,
                                                                keep = IEATools::template_cols$eta_fu,
                                                                countries = Countries)),
                            pattern = quote(map(Countries))),

    # (11d) Exergy-to-energy ratio (phi_u) vectors at the useful stage
    targets::tar_target_raw("Phiuvecs", quote(sep_eta_fu_phi_u(EtafuPhiuvecs,
                                                               keep = IEATools::template_cols$phi_u,
                                                               countries = Countries)),
                            pattern = quote(map(Countries))),

    # (11e) Exergy-to-energy ratio (phi_pf) vectors at the primary and final stages
    targets::tar_target_raw("Phipfvecs", quote(calc_phi_pf_vecs(phi_constants = PhiConstants,
                                                                phi_u_vecs = Phiuvecs,
                                                                countries = Countries, 
                                                                matrix_class = matrix_class)),
                            pattern = quote(map(Countries))),

    # (11f) Exergy-to-energy ratio (phi) vectors at all stages
    targets::tar_target_raw("Phivecs", quote(sum_phi_vecs(phi_pf_vecs = Phipfvecs,
                                                          phi_u_vecs = Phiuvecs,
                                                          countries = Countries)),
                            pattern = quote(map(Countries))),

    # (12) Extend to useful stage
    targets::tar_target_raw("PSUTUsefulIEA", quote(move_to_useful(psut_final = PSUTFinalIEA,
                                                                  C_mats = Cmats,
                                                                  eta_phi_vecs = EtafuPhiuvecs,
                                                                  countries = Countries)),
                            pattern = quote(map(Countries))),

    # (13) Add other methods


    # (14) Add exergy quantifications of energy
    targets::tar_target_raw("PSUTIEA", quote(move_to_exergy(psut_energy = PSUTUsefulIEA,
                                                            phi_vecs = Phivecs,
                                                            countries = Countries)),
                            pattern = quote(map(Countries))),


    # (15) Make PSUT matrices from muscle work data
    targets::tar_target_raw("PSUTMW_energy", quote(make_mw_psut(.hmw_df = HMWPFUData,
                                                                .amw_df = AMWPFUData,
                                                                matrix_class = matrix_class,
                                                                countries = Countries,
                                                                years = Years)),
                            pattern = quote(map(Countries))),
    # Ensure that the MW data are balanced
    targets::tar_target_raw("BalancedPSUTMW", quote(verify_mw_energy_balance(PSUTMW_energy, countries = Countries)),
                            pattern = quote(map(Countries))),
    # Don't continue if there is a problem with the MW data.
    # stopifnot returns NULL if everything is OK.
    targets::tar_target_raw("OKToProceedMW", quote(ifelse(is.null(stopifnot(BalancedPSUTMW)), yes = TRUE, no = FALSE))),


    # (16) Move from energy to exergy for muscle work
    # Create a single phi vector applicable to all years.
    targets::tar_target_raw("PhivecMW", quote(MWTools::phi_vec_mw(.phi_table = PhiConstants,
                                                                  mw_energy_carriers = MWTools::mw_products, 
                                                                  matrix_class = matrix_class))),
    # This target has a phi vector for every Country-Year combination.
    # Note the plural spelling.
    targets::tar_target_raw("PhivecsMW", quote(calc_phi_vecs_mw(psut_energy_mw = PSUTMW_energy,
                                                                phi_vec_mw = PhivecMW,
                                                                countries = Countries)),
                            pattern = quote(map(Countries))),
    targets::tar_target_raw("PSUTMW_all_years", quote(move_to_exergy(psut_energy = PSUTMW_energy,
                                                                     phi_vecs = PhivecsMW,
                                                                     countries = Countries)),
                            pattern = quote(map(Countries))),

    # (17) Trim MW to years also available in IEA
    targets::tar_target_raw("PSUTMW", quote(filter_mw_to_iea_years(PSUTMW_all_years, PSUTIEA, 
                                                                   countries = Countries)), 
                            pattern = quote(map(Countries))),

    # (18) Combine IEA and MW data by summing PSUT matrices
    targets::tar_target_raw("PSUTIEAMW", quote(add_iea_mw_psut(PSUTIEA, PSUTMW,
                                                               countries = Countries)),
                            pattern = quote(map(Countries))),


    # (19) Build final data frame
    targets::tar_target_raw("PSUT", quote(build_psut_dataframe(psutiea = PSUTIEA,
                                                               psutmw = PSUTMW,
                                                               psutieamw = PSUTIEAMW))),

    # (20) Calculate final-to-useful efficiencies from f-u allocations and machine efficiencies
    targets::tar_target_raw("EtafuvecsYEIOU", quote(calc_fu_Y_EIOU_efficiencies(C_mats = Cmats,
                                                                                eta_fu_vecs = Etafuvecs,
                                                                                phi_vecs = Phivecs,
                                                                                countries = Countries)),
                            pattern = quote(map(Countries))),

    # (30) Build reports
    # (30a) Allocation Graphs
    targets::tar_target_raw("AllocationGraphs", quote(alloc_plots_df(CompletedAllocationTables, countries = Countries)),
                            pattern = quote(map(Countries))),
    # (30b) Non-Stationary Allocation Graphs
    targets::tar_target_raw("NonStationaryAllocationGraphs", quote(nonstat_alloc_plots_df(CompletedAllocationTables, countries = Countries)),
                            pattern = quote(map(Countries))),
    # (30c) Efficiency Graphs
    targets::tar_target_raw("EfficiencyGraphs", quote(eta_fu_plots_df(CompletedEfficiencyTables, countries = Countries)),
                            pattern = quote(map(Countries))),
    # (30d) Exergy-to-energy ratio graphs
    targets::tar_target_raw("PhiGraphs", quote(phi_u_plots_df(CompletedEfficiencyTables, countries = Countries)),
                            pattern = quote(map(Countries))),

    # (31) Save results
    # (31a) Pin the PSUT data frame
    targets::tar_target_raw("ReleasePSUT", quote(release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                                                targ = PSUT,
                                                                pin_name = "psut",
                                                                release = Release))), 
    
    # Some database products are best suited to creating and storing here.
    
    # --------------------------------------------------------------------------
    # Product A ----------------------------------------------------------------
    # --------------------------------------------------------------------------
    # Pin the PSUT_USA data frame ----------------------------------------------
    
    # Filter to the US for Carey King
    targets::tar_target_raw(
      "PSUT_USA",
      quote(PSUT %>%
                   dplyr::filter(Country == "USA"))
    ),
    
    targets::tar_target_raw(
      "ReleasePSUT_USA",
      quote(release_target(pipeline_releases_folder = PipelineReleasesFolder,
                           targ = PSUT_USA,
                           pin_name = "psut_usa",
                           release = Release))
    ),
    
    
    # --------------------------------------------------------------------------
    # Product B ----------------------------------------------------------------
    # --------------------------------------------------------------------------
    # Final-to-useful sector-carrier efficiencies ------------------------------
    
    targets::tar_target_raw(
      "EtafuYEIOU", 
      quote(calc_fu_Y_EIOU_efficiencies(C_mats = Cmats, 
                                        eta_fu_vecs = Etafuvecs, 
                                        phi_vecs = Phivecs, 
                                        countries = Countries)), 
      pattern = quote(map(Countries))), 
    
    targets::tar_target_raw(
      "ReleaseEtafuYEIOU",
      quote(release_target(pipeline_releases_folder = PipelineReleasesFolder,
                           targ = EtafuYEIOU,
                           pin_name = "eta_fu_Y_eiou",
                           release = Release))
    ), 
    
    
    # Zip the targets cache and store it in the pipeline_caches_folder
    targets::tar_target_raw("StoreCache", quote(stash_cache(pipeline_caches_folder = PipelineCachesFolder,
                                                            cache_folder = "_targets",
                                                            file_prefix = "pfu_pipeline_cache_",
                                                            dependency = PSUT_USA, 
                                                            release = Release)))
  )
}