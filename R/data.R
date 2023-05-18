#' Targets cache target names
#'
#' A string list containing names of `targets` targets.
#'
#' @format A string list with `r length(target_names)` entries.
#' \describe{
#' \item{countries}{The countries to be analyzed in this pipeline.}
#' \item{additional_exemplar_countries}{The additional exemplar countries from which allocations and efficiencies can be obtained.}
#' \item{alloc_and_eff_countries}{The allocation and efficiency countries target in the `targets` cache, giving the countries from which final-to-useful allocations and efficiencies can be drawn for final-to-useful analyses. This is a superset of countries to be analyzed.}
#' \item{years}{The years to be analyzed.}
#' \item{iea_data_path}{The path to the file containing IEA extended energy balance data.}
#' \item{country_concordance_path}{The path to the country concordance file.}
#' \item{phi_constants_path}{The path to exergy-to-energy ration (phi) constants.}
#' \item{ceda_data_folder}{The path to the folder containing CEDA data.}
#' \item{machine_data_path}{The path to the folder containing machine data excel files.}
#' \item{exemplar_table_path}{The path to the exemplar table.}
#' \item{fu_analysis_folder}{The path to a folder containing final-to-useful analyses for various countries.}
#' \item{report_source_folders}{The path to a folder containing reports to be run as the last step of the pipeline.}
#' \item{report_dest_folder}{The path to a folder containing reports from the pipeline.}
#' \item{pipeline_caches_folder}{The path to a folder containing pipeline caches for each release.}
#' \item{pipeline_releases_folder}{The path to a folder containing releases of targets.}
#' \item{release}{A boolean telling whether a release was requested.}
#' \item{iea_data}{The IEA data for `countries`.}
#' \item{country_concordance_table}{A data frame containing concordance information which maps full country names to custom 3 letter codes.}
#' \item{ceda_data}{The name of the data frame containing all CEDA temperature data read from `ceda_data_folder`.}
#' \item{all_machine_data}{A data frame containing eta_fu values.}
#' \item{machine_data}{A filtered version of `all_machine_data` containing information for only `alloc_and_eff_countries`.}
#' \item{socio_econ_data}{A data frame containing socioeconomic data, supplied by `get_L_K_GDP_data()`.}
#' \item{balanced_before}{A boolean indicating whether the `iea_data` are balanced before any further analysis. They usually are not, so this value is typically `FALSE`.}
#' \item{balanced_iea_data}{A balanced version of `iea_data`.}
#' \item{balanced_after}{Same as `balanced_before`, only for after balancing. This should always be be `TRUE`.}
#' \item{ok_to_proceed}{A boolean telling whether we can continue the pipeline. This target errors if `balanced_after` is not `TRUE`.}
#' \item{specified}{A data frame containing specified IEA data.}
#' \item{psut_final}{A data frame containing `specified` in a PSUT format.}
#' \item{exemplar_lists}{A data frame of lists of exemplar countries for each country in `countries`, and maybe more.}
#' \item{phi_constants}{A data frame of constant values for exergy-to-energy ratio (phi).}
#' \item{incomplete_allocation_tables}{A data frame of final-to-useful allocation tables, one for each country. These allocation tables may be incomplete.}
#' \item{completed_allocation_tables}{A data frame of completed final-to-useful allocation tables.}
#' \item{completed_efficiency_tables}{A data frame of completed final-to-useful efficiency tables.}
#' \item{completed_phi_u_tables}{A data frame of completed useful-stage exergy-to-energy ratios.}
#' \item{cmats}{A data frame containing `CompletedAllocationTables` in matrix form.}
#' \item{eta_fu_phi_u_vecs}{A data frame containing final-to-useful efficiency vectors and useful exergy-to-energy ratios.}
#' \item{eta_fu_vecs}{A data frame containing final-to-useful efficiency vectors.}
#' \item{phi_u_vecs}{A data frame containing useful exergy-to-energy ratio vectors.}
#' \item{phi_pf_vecs}{A data frame containing primary and final exergy-to-energy ratio vectors.}
#' \item{phi_vecs}{A data frame containing exergy-to-energy ratio vectors.}
#' \item{psut_useful}{A data frame containing PSUT matrices up to the useful stage.}
#' \item{psut}{A data frame containing PSUT matrices up to the useful stage and with exergy.}
#' \item{allocation_graphs}{A data frame containing final-to-useful allocation graphs.}
#' \item{non_stationary_allocation_graphs}{A data frame containing final-to-useful allocation graphs, for non-stationary data only.}
#' \item{efficiency_graphs}{A data frame containing final-to-useful efficiency graphs.}
#' \item{phi_graphs}{A data frame containing exergy-to-energy ratio (phi) graphs.}
#' \item{release_psut}{A target that does a release of the `psut` target. Contains the name of the target or a message saying that a release was not requested.}
#' \item{store_cache}{A target that stores the targets cache. Contains the name of the target or a message saying that a release was not requested.}
#' }
#'
#' @examples
#' target_names
"target_names"


#' Information about the machine efficiency files
#'
#' A string list containing information about machine efficiency files.
#' Items in the list provide default values for machine efficiency files,
#' including Excel tab names, etc.
#'
#' @format A string list with `r length(machine_constants)` entries.
#' \describe{
#' \item{efficiency_tab_name}{The default name of the efficiency tabs in machine efficiency excel files.}
#' }
#'
#' @examples
#' machine_constants
"machine_constants"


#' Constants for socioeconomic data files
#'
#' A string list containing values for features of socioeconomic data files.
#'
#' @format A string list with `r length(socioecon_cols)` entries.
#' \describe{
#' \item{isocode_colname}{The name of the column containing ISO country codes, "isocode".}
#' \item{year_colname}{The name of the column containing years, "Year".}
#' \item{rgdpe_colname}{The name of the column containing rgdpe values, "rgdpe".}
#' \item{rgdpo_colname}{The name of the column containing rgdpo values, "rgdpo".}
#' \item{rgdpna_colname}{The name of the column containing rgdpna values, "rgdpna".}
#' \item{emp_colname}{The name of the column containing employment values, "emp".}
#' \item{avh_colname}{The name of the column containing avh values, "avh".}
#' \item{hc_colname}{The name of the column containing hc values, "hc".}
#' \item{rnna_colname}{The name of the column containing rnna values, "rnna".}
#' \item{rkna_colname}{The name of the column containing rkna values, "rkna".}
#' \item{K_colname}{The name of the column containing capital values, "K".}
#' \item{Kserv_colname}{The name of the column containing capital services values, "Kserv".}
#' \item{L_colname}{The name of the column containing labor values, "L".}
#' \item{Ladj_colname}{The name of the column containing quality-adjusted labor values, "Ladj".}
#' }
#'
#' @examples
#' socioecon_cols
"socioecon_cols"


#' Constants for data frames containing IEA and MW data frames
#'
#' A string list containing both the column name and column values
#' for energy conversion chains (ECCs) in PSUT format.
#'
#' @format A string list with `r length(ieamw_cols)` entries.
#' \describe{
#' \item{ieamw}{The name of the column containing metadata on ECC sources. "IEAMW"}
#' \item{iea}{A string identifying that ECC data are from the IEA exclusively. "IEA"}
#' \item{mw}{A string identifying that ECC data are for muscle work (MW) exclusively. "MW"}
#' \item{both}{A string identifying that ECC data include both IEA and muscle work. "Both"}
#' }
#'
#' @examples
#' ieamw_cols
"ieamw_cols"


#' All countries and selected country groups
#'
#' All individual countries and selected country groups from the IEA's World Extended
#' energy balances 2021.
#'
#' @format A string list with `r length(all_countries)` entries.
#'
#' @examples
#' all_countries
"all_countries"


#' Double-counted countries
#'
#' Selected individual countries and selected country groups which, if aggregated,
#' would result in double counting.
#'
#' @format A string list with `r length(double_counted_countries)` entries.
#'
#' @examples
#' double_counted_countries
"double_counted_countries"


#' Canonical countries
#'
#' Countries and country groups which, if aggregated, cover the entire world
#' (WRLD) without double counting.
#'
#' @format A string list with `r length(canonical_countries)` entries.
#'
#' @examples
#' canonical_countries
"canonical_countries"


#' Exemplar table names
#'
#' A string list containing named names of columns and tabs for exemplar tables.
#' Items in the list provide default values for column name function arguments
#' throughout the `PFUDatabase` package.
#'
#' @format A string list with `r length(exemplar_names)` entries.
#' \describe{
#' \item{exemplar_tab_name}{The string name of the tab in the Excel file containing the exemplar table.}
#' \item{prev_names}{The name of a column of previous names used for the country.}
#' \item{exemplars}{The name of a column of exemplar countries.}
#' \item{exemplar_country}{The name of an exemplar country column.}
#' \item{exemplar_countries}{The name of an exemplar countries column.}
#' \item{exemplar_tables}{The name of a column containing exemplar tables.}
#' \item{iea_data}{The name of a column containing IEA extended energy balance data.}
#' \item{alloc_data}{The name of a column containing final-to-useful allocation data.}
#' \item{incomplete_alloc_table}{The name of a column containing incomplete final-to-useful allocation tables.}
#' \item{complete_alloc_table}{The name of a column containing completed final-to-useful allocation tables.}
#' \item{incomplete_eta_table}{The name of a column containing incomplete final-to-useful efficiency tables.}
#' \item{complete_eta_table}{The name of a column containing completed final-to-useful efficiency tables.}
#' \item{region_code}{The name of the region code column.}
#' \item{country_name}{The name of the column containing the long name of a country.}
#' \item{agg_code_col}{The metadata column "Agg.Code", representing the country, or country group code for individual country level data to be aggregated in to.}
#' \item{world}{The name of the world region.}
#' }
#'
#' @examples
#' exemplar_names
"exemplar_names"


#' Sources for phi values
#'
#' A string list containing named sources of phi (exergy-to-energy ratio) values.
#'
#' @format A string list with `r length(phi_sources)` entries.
#' \describe{
#' \item{eta_fu_tables}{Tables of final-to-useful efficiency values.}
#' \item{temperature_data}{Country-average yearly temperature data.}
#' \item{phi_constants}{Tables of constant phi values.}
#' }
#'
#' @examples
#' phi_sources
"phi_sources"
