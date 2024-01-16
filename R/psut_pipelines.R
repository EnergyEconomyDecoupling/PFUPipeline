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
#' @param ilo_employment_data_path The path to International Labor Organization (ILO) employment data.
#' @param ilo_working_hours_data_path The path to International Labor Organization (ILO) working hours data.
#' @param machine_data_path The path to the machine data in .xlsx format.
#' @param exemplar_table_path The path to an exemplar table.
#' @param fu_analysis_folder The path to a folder containing final-to-useful analyses.
#'                           Sub-folders named with 3-letter country abbreviations are assumed.
#' @param exiobase_energy_flows_path The path to the file where the list of Exiobase energy flows, and their concordance to the PFU database flows are stored.
#' @param years_exiobase The years for which the multipliers to provide to the Exiobase team need to be calculated.
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
                         ilo_employment_data_path,
                         ilo_working_hours_data_path,
                         machine_data_path,
                         exemplar_table_path,
                         fu_analysis_folder,
                         exiobase_energy_flows_path,
                         years_exiobase,
                         reports_source_folders,
                         reports_dest_folder,
                         pipeline_releases_folder,
                         pipeline_caches_folder,
                         release = FALSE) {
  
  # Avoid R CMD check errors
  PSUT <- NULL
  Year <- NULL
  Cmats <- NULL
  Country <- NULL
  
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
    targets::tar_target_raw("ILOEmploymentDataPath", ilo_employment_data_path),
    targets::tar_target_raw("ILOWorkingHoursDataPath", ilo_working_hours_data_path),
    targets::tar_target_raw("MachineDataPath", machine_data_path),
    targets::tar_target_raw("ExemplarTablePath", exemplar_table_path),
    targets::tar_target_raw("FUAnalysisFolder", fu_analysis_folder),
    targets::tar_target_raw("ReportsSourceFolders", reports_source_folders),
    targets::tar_target_raw("ReportsDestFolder", reports_dest_folder),
    targets::tar_target_raw("PipelineReleasesFolder", pipeline_releases_folder),
    targets::tar_target_raw("PipelineCachesFolder", pipeline_caches_folder),
    targets::tar_target_raw("Release", release),
    
    # Exiobase information
    targets::tar_target_raw("ExiobaseEnergyFlowsPath", exiobase_energy_flows_path),
    targets::tar_target_raw("ExiobaseYears", list(years_exiobase)),
    
    
    # (1) Load pipeline information

    # (1a) IEA data
    targets::tar_target_raw("AllIEAData", quote(IEATools::load_tidy_iea_df(IEADataPath, 
                                                                           override_df = CountryConcordanceTable, 
                                                                           specify_non_energy_flows = SpecifyNonEnergyFlows, 
                                                                           apply_fixes = ApplyFixes))),
    targets::tar_target_raw("IEAData", quote(AllIEAData %>%
                                               PFUPipelineTools::filter_countries_years(countries = AllocAndEffCountries, years = Years))),

    # (1b) Country concordance table
    targets::tar_target_raw("CountryConcordanceTable", quote(load_country_concordance_table(country_concordance_path = CountryConcordancePath))),

    # Temperature data not required for V1
    # (1c) CEDA data for ALL countries
    # targets::tar_target_raw("CEDAData", quote(CEDATools::create_agg_cru_cy_df(agg_cru_cy_folder = CEDADataFolder,
    #                                                                           agg_cru_cy_metric = c("tmp", "tmn", "tmx"),
    #                                                                           agg_cru_cy_year = 2020))),

    targets::tar_target_raw(
      "AllMachineData", 
      quote(read_all_eta_files(eta_fin_paths = get_eta_filepaths(MachineDataPath)))
    ),
    
    
    targets::tar_target_raw(
      "MachineData", 
      quote(PFUPipelineTools::filter_countries_years(AllMachineData, countries = AllocAndEffCountries, years = Years))
    ),

    # (1e) Muscle work data
    targets::tar_target_raw("AMWPFUDataRaw", quote(load_amw_pfu_data(fao_data_path = FAODataPath,
                                                                     mw_concordance_path = MWConcordancePath,
                                                                     amw_analysis_data_path = AMWAnalysisDataPath) |>
                                                     PFUPipelineTools::filter_countries_years(countries = AllocAndEffCountries, years = Years))),

    targets::tar_target_raw("HMWPFUDataRaw", quote(load_hmw_pfu_data(ilo_working_hours_data_path = ILOWorkingHoursDataPath,
                                                                     ilo_employment_data_path = ILOEmploymentDataPath,
                                                                     mw_concordance_path = MWConcordancePath,
                                                                     hmw_analysis_data_path = HMWAnalysisDataPath) |>
                                                     PFUPipelineTools::filter_countries_years(countries = AllocAndEffCountries, years = Years))),

    targets::tar_target_raw("AMWPFUData", quote(aggcountries_mw_to_iea(mw_df = AMWPFUDataRaw,
                                                                       exemplar_table_path = ExemplarTablePath))),

    targets::tar_target_raw("HMWPFUData", quote(aggcountries_mw_to_iea(mw_df = HMWPFUDataRaw,
                                                                       exemplar_table_path = ExemplarTablePath))),

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

    
    # 
    # Product D: completed_allocation_tables -----------------------------------
    # 
    targets::tar_target_raw("CompletedAllocationTables", 
                            quote(assemble_fu_allocation_tables(incomplete_allocation_tables = IncompleteAllocationTables,
                                                                exemplar_lists = ExemplarLists,
                                                                specified_iea_data = SpecifiedIEA %>% dplyr::mutate(tar_group = NULL),
                                                                countries = Countries,
                                                                years = Years)),
                            pattern = quote(map(Countries))),
    
    targets::tar_target_raw(
      "ReleaseCompletedAllocationTables",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = CompletedAllocationTables,
                                             pin_name = "completed_allocation_tables",
                                             release = Release))
    ),
    
    
    #
    # Product E: completed_efficiency_tables -----------------------------------
    #
    targets::tar_target_raw("CompletedEfficiencyTables", quote(assemble_eta_fu_tables(incomplete_eta_fu_tables = MachineData,
                                                                                      exemplar_lists = ExemplarLists,
                                                                                      completed_fu_allocation_tables = CompletedAllocationTables,
                                                                                      countries = Countries,
                                                                                      years = Years,
                                                                                      which_quantity = IEATools::template_cols$eta_fu)),
                            pattern = quote(map(Countries))),
    
    targets::tar_target_raw(
      "ReleaseCompletedEfficiencyTables",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = CompletedEfficiencyTables,
                                             pin_name = "completed_efficiency_tables",
                                             release = Release))
    ),
    
    

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

    # 
    # Product F: phi_vecs ------------------------------------------------------
    # 
    targets::tar_target_raw("Phivecs", quote(sum_phi_vecs(phi_pf_vecs = Phipfvecs,
                                                          phi_u_vecs = Phiuvecs,
                                                          countries = Countries)),
                            pattern = quote(map(Countries))),
    targets::tar_target_raw(
      "ReleasePhivecs",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = Phivecs,
                                             pin_name = "phi_vecs",
                                             release = Release))
    ),
    

    # (12) Extend to useful stage
    
    # (12.1) Calculate with detailed matrices
    targets::tar_target_raw("PSUTUsefulIEAWithDetails", 
                            quote(move_to_useful_with_details(psut_final = PSUTFinalIEA,
                                                              C_mats = Cmats,
                                                              eta_phi_vecs = EtafuPhiuvecs,
                                                              countries = Countries)),
                            pattern = quote(map(Countries))),
    
    # (12.2) Keep only the PSUT matrices for the energy conversion chains
    targets::tar_target_raw("PSUTUsefulIEA",
                            quote(PSUTUsefulIEAWithDetails |> 
                                    dplyr::select(-dplyr::any_of(c(IEATools::psut_cols$Y_fu_detailed, 
                                                                   IEATools::psut_cols$U_eiou_fu_detailed)))),
                            pattern = quote(map(Countries))),
    
    # (12.3) Keep the detailed matrices for another product
    targets::tar_target_raw("YfuUEIOUfudetailed", 
                            quote(PSUTUsefulIEAWithDetails |> 
                                    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] == IEATools::all_stages$useful) |> 
                                    dplyr::select(-dplyr::any_of(c(IEATools::psut_cols$R, 
                                                                   IEATools::psut_cols$U,
                                                                   IEATools::psut_cols$U_feed,
                                                                   IEATools::psut_cols$U_eiou,
                                                                   IEATools::psut_cols$r_eiou,
                                                                   IEATools::psut_cols$V,
                                                                   IEATools::psut_cols$Y,
                                                                   IEATools::psut_cols$s_units)))),
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


    
    # 
    # Product A: psut ---------------------------------------------------------- 
    # 
    targets::tar_target_raw("PSUT", quote(build_psut_dataframe(psutiea = PSUTIEA,
                                                               psutmw = PSUTMW,
                                                               psutieamw = PSUTIEAMW))
    ),
    targets::tar_target_raw("ReleasePSUT", 
                            quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                                                   targ = PSUT,
                                                                   pin_name = "psut",
                                                                   release = Release))), 
    
    
    # Make a version that is grouped by Year for later parallel calculations
    tarchetypes::tar_group_by(
      name = "PSUTbyYear",
      command = PSUT,
      Year
    ),
    
    
    # 
    # Product B: psut_usa ----------------------------------------------------------------
    # 
    targets::tar_target_raw(
      "PSUT_USA",
      quote(PSUT |> 
              dplyr::filter(Country == "USA"))
    ),
    targets::tar_target_raw(
      "ReleasePSUT_USA",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = PSUT_USA,
                                             pin_name = "psut_usa",
                                             release = Release))
    ),
    
    
    # (20) Calculate final-to-useful efficiencies from f-u allocations and machine efficiencies
    targets::tar_target_raw("EtafuvecsYEIOU", 
                            quote(calc_fu_Y_EIOU_efficiencies(C_mats = Cmats,
                                                              eta_fu_vecs = Etafuvecs,
                                                              phi_vecs = Phivecs,
                                                              countries = Countries)),
                            pattern = quote(map(Countries))),

    # (21) Calculating Cmats (i) EIOU-wide, (ii) Y-wide, and (iii) economy-wide
    # Add parallelisation later
    tarchetypes::tar_group_by(
      name = "CmatsbyCountry",
      command = Cmats,
      # Change later to only Country. 
      # Country, Year was only for diagnostic purposes.
      Country, Year
    ),
    targets::tar_target_raw("CmatsAgg",
                            quote(calc_C_mats_agg(C_mats = CmatsbyCountry,
                                                  psut_iea = PSUTIEA))#,
                            #pattern = quote(map(CmatsbyCountry))
    ),
    
    
    
    
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
    
  
    
    # 
    # Product C: eta_fu_Y_eiou -------------------------------------------------
    # 
    targets::tar_target_raw(
      "EtafuYEIOU", 
      quote(calc_fu_Y_EIOU_efficiencies(C_mats = Cmats, 
                                        eta_fu_vecs = Etafuvecs, 
                                        phi_vecs = Phivecs, 
                                        countries = Countries)), 
      pattern = quote(map(Countries))
    ), 
    targets::tar_target_raw(
      "ReleaseEtafuYEIOU",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = EtafuYEIOU,
                                             pin_name = "eta_fu_Y_eiou",
                                             release = Release))
    ), 
    
    
    
    
    # 
    # Product G: eta_i ---------------------------------------------------------
    # 
    targets::tar_target_raw(
      "Etai",
      quote(PSUTbyYear |>
              Recca::calc_eta_i() |> 
              dplyr::mutate(
                R = NULL, U = NULL, U_feed = NULL, U_EIOU = NULL, 
                r_EIOU = NULL, V = NULL, Y = NULL, S_units = NULL
              ) |> 
              PFUPipelineTools::tar_ungroup()),
      pattern = quote(map(PSUTbyYear))
    ),
    targets::tar_target_raw(
      "ReleaseEtai",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = Etai,
                                             pin_name = "eta_i",
                                             release = Release))),

        
    # 
    # Product H: exiobase_Ef_to_Eloss_multipliers ------------------------------
    # 
    # (1 - Eta_fu) values
    # Multiplier to go from final energy to energy losses
    targets::tar_target_raw(
      "ExiobaseEftoElossMultipliers",
      quote(calc_Ef_to_Eloss_exiobase(ExiobaseEftoEuMultipliers))
    ),
    targets::tar_target_raw(
      "ReleaseExiobaseEftoElossMultipliers",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = ExiobaseEftoElossMultipliers,
                                             pin_name = "exiobase_Ef_to_Eloss_multipliers",
                                             type = "csv",
                                             release = Release))),
    
    
    # 
    # Product I: exiobase_Ef_to_Eu_multipliers ---------------------------------
    # 
    # Calculating the product efficiency at the (i) EIOU-wide, (ii) Y-wide, and (iii) economy-wide levels
    # Add parallelisation later
    targets::tar_target_raw(
      "EtafuYEIOUagg",
      quote(calc_fu_Y_EIOU_agg_efficiencies(C_mats_agg = CmatsAgg, 
                                            eta_fu_vecs = Etafuvecs,
                                            phi_vecs = Phivecs))
    ),
    # Eta_fu, E, values
    # Multiplier to go from final energy to useful energy
    targets::tar_target_raw(
      "ExiobaseEftoEuMultipliers",
      quote(calc_Ef_to_Eu_exiobase(eta_fu_Y_EIOU_mats = EtafuYEIOU,
                                   eta_fu_Y_EIOU_agg = EtafuYEIOUagg,
                                   years_exiobase = ExiobaseYears,
                                   full_list_exiobase_flows = ListExiobaseEnergyFlows,
                                   country_concordance_table_df = CountryConcordanceTable))
    ),
    targets::tar_target_raw(
      "ReleaseExiobaseEftoEuMultipliers",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = ExiobaseEftoEuMultipliers,
                                             pin_name = "exiobase_Ef_to_Eu_multipliers",
                                             type = "csv",
                                             release = Release))),
    
    
    # 
    # Product J: exiobase_Ef_to_Xf_multipliers ---------------------------------
    # 
    # Final energy to final exergy multipliers
    # List of Exiobase code energy flows
    targets::tar_target_raw(
      "ListExiobaseEnergyFlows",
      quote(read_list_exiobase_energy_flows(path_to_list_exiobase_energy_flows = ExiobaseEnergyFlowsPath))
    ),
    
    # Phi values
    # Multiplier to go from final energy to final exergy
    targets::tar_target_raw(
      "ExiobaseEftoXfMultipliers",
      quote(calc_Ef_to_Xf_exiobase(phi_vecs = Phivecs,
                                   years_exiobase = ExiobaseYears,
                                   full_list_exiobase_flows = ListExiobaseEnergyFlows,
                                   country_concordance_table_df = CountryConcordanceTable))
    ),
    # Release
    targets::tar_target_raw(
      "ReleaseExiobaseEftoXfMultipliers",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = ExiobaseEftoXfMultipliers,
                                             pin_name = "exiobase_Ef_to_Xf_multipliers",
                                             type = "csv",
                                             release = Release))),
    
    
    # 
    # Product K: exiobase_Ef_to_Xloss_multipliers ------------------------------
    # 
    # Final energy to exergy losses multipliers
    # Multiplier to go from final energy to exergy losses
    targets::tar_target_raw(
      "ExiobaseEftoXlossMultipliers",
      quote(calc_Ef_to_Xloss_exiobase(ExiobaseEftoXuMultipliers))
    ),
    targets::tar_target_raw(
      "ReleaseExiobaseEftoXlossMultipliers",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = ExiobaseEftoXlossMultipliers,
                                             pin_name = "exiobase_Ef_to_Xloss_multipliers",
                                             type = "csv",
                                             release = Release))), 

    # 
    # Product L: exiobase_Ef_to_Xu_multipliers ---------------------------------
    # 
    # Final energy to useful exergy multipliers
    # Multiplier to go from final exergy to useful exergy
    # This is just an intermediary target that is needed for the ExiobaseEftoXuMultipliers targets
    targets::tar_target_raw(
      "EtafuPhiYEIOUagg", 
      quote(calc_eta_fu_eff_phi_Y_EIOU_agg(C_mats_agg = CmatsAgg,
                                           eta_fu_vecs = Etafuvecs,
                                           phi_vecs = Phivecs))
    ),
    targets::tar_target_raw(
      "ExiobaseEftoXuMultipliers",
      quote(calc_Ef_to_Xu_exiobase(EtafuYEIOU_mats = EtafuYEIOU,
                                   phi_vecs = Phivecs,
                                   eta_fu_phi_Y_EIOU_agg = EtafuPhiYEIOUagg,
                                   years_exiobase = ExiobaseYears,
                                   full_list_exiobase_flows = ListExiobaseEnergyFlows,
                                   country_concordance_table_df = CountryConcordanceTable))
    ),
    targets::tar_target_raw(
      "ReleaseExiobaseEftoXuMultipliers",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = ExiobaseEftoXuMultipliers,
                                             pin_name = "exiobase_Ef_to_Xu_multipliers",
                                             type = "csv",
                                             release = Release))),
    
    # 
    # Product M: psut_without_neu ----------------------------------------------
    # 
    # Calculate a version of the PSUT data frame with all Non-energy use removed.
    # This target is parallelized.
    targets::tar_target_raw(
      "PSUTWithoutNEU", 
      quote(remove_non_energy_use(PSUTbyYear)), 
      pattern = quote(map(PSUTbyYear))
    ), 
    targets::tar_target_raw(
      "ReleasePSUTWithoutNEU",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = PSUTWithoutNEU,
                                             pin_name = "psut_without_neu",
                                             release = Release))), 
    
    # 
    # Product N: all_machine_data ----------------------------------------------
    # 
    # Information on all machines in the database.
    targets::tar_target_raw(
      "ReleaseAllMachineData",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = AllMachineData,
                                             pin_name = "all_machine_data",
                                             release = Release))
    ),
    
    # 
    # Product O: Y_fu_U_EIOU_fu_detailed ---------------------------------------
    # 
    # A matsindf data frame of detailed matrices describing
    # the move from final to useful energy
    targets::tar_target_raw(
      "ReleaseYfuUEIOUfudetailed",
      quote(PFUPipelineTools::release_target(pipeline_releases_folder = PipelineReleasesFolder,
                                             targ = AllMachineData,
                                             pin_name = "Y_fu_U_EIOU_fu_detailed",
                                             release = Release))
    )

    
    # Zip the targets cache and store it in the pipeline_caches_folder
    # Commented in Dec 2023, because we don't use it.
    # targets::tar_target_raw("StoreCache", 
    #                         quote(PFUPipelineTools::stash_cache(pipeline_caches_folder = PipelineCachesFolder,
    #                                                             cache_folder = "_targets",
    #                                                             file_prefix = "pfu_pipeline_cache_",
    #                                                             dependency = c(ReleasePSUT, ReleasePSUT_USA,
    #                                                                            ReleaseEtafuYEIOU, ReleaseCompletedAllocationTables,
    #                                                                            ReleaseCompletedEfficiencyTables, CompletedPhiTables,
    #                                                                            ReleaseEtai),
    #                                                             release = Release))
    # )
  ) 
}


