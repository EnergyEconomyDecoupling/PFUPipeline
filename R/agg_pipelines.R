#' Title
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
#'                                 for later retrieval as pinned items on a pinboard..
#' @param release A boolean that tells whether a new release of the `PSUT` target should be made.
#'                Default is `FALSE`.
#'
#' @return A `targets` pipeline.
#' 
#' @export
get_pipeline <- function(countries,
                         additional_exemplar_countries = NULL,
                         years,
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
    
    # Identify the countries for this analysis.
    # "all" means all countries.
    targets::tar_target_raw(
      name = "countries",
      command = countries
    )
    
    
  )
}