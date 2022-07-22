#' Calculate PSUT data frames from muscle work PFU data frames
#'
#' This function filters to `countries` and creates PSUT matrices 
#' from primary-final-useful muscle work data frames.
#'
#' @param .hmw_df,.amw_df Data frames of primary-final-useful muscle work data.
#' @param countries The countries to be analyzed. 
#' @param years The years to be analyzed.
#' @param country The name of the country column in `.hmw_df` and `.amw_df`. Default is `MWTools::mw_cols$country`.
#' @param year The name of the year column in `.hmw_df` and `.amw_df`.Default is `MWTools::mw_cols$year`.
#'
#' @return A data frame of PSUT matrices for a muscle work energy conversion chain.
#' 
#' @export
make_mw_psut <- function(.hmw_df, .amw_df,
                         countries,
                         years,
                         country = MWTools::mw_cols$country, 
                         year = MWTools::mw_cols$year) {
  
  hmw_data <- .hmw_df %>% 
    dplyr::filter(.data[[country]] %in% countries, 
                  .data[[year]] %in% years)
  amw_data <- .amw_df %>% 
    dplyr::filter(.data[[country]] %in% countries, 
                  .data[[year]] %in% years)
  MWTools::prep_psut(.hmw_df = hmw_data, .amw_df = amw_data)
}


#' Verify energy balance in muscle work PSUT matrices
#' 
#' After constructing the muscle work PSUT matrices, 
#' energy balance should be verified. 
#' Internally, this function uses `Recca::verify_SUT_energy_balance()`
#' to ensure that everything is balanced.
#' 
#' If `.psut_df` has zero rows, 
#' `TRUE` is returned, enabling the pipeline to continue,
#' even if there are some years where there is no muscle work data available.
#'
#' @param .psut_df A data frame of muscle work PSUT matrices.
#' @param countries The countries to be analyzed. 
#' @param country The name of the country column in `.psut_df`.
#'
#' @return A data frame with new boolean column ".balanced" that tells
#'         whether the matrices are balanced.
#' 
#' @export
verify_mw_energy_balance <- function(.psut_df, 
                                     countries, 
                                     country = MWTools::mw_cols$country) {
  if (nrow(.psut_df) == 0) {
    return(TRUE)
  }
  filtered_df <- .psut_df %>% 
    dplyr::filter(.data[[country]] %in% countries) 
  # Check that we have some rows.
  if (nrow(filtered_df) == 0) {
    # If we have not rows, still return true.
    return(TRUE)
  }
  # We have some rows. Perform the check.
  filtered_df %>% 
    Recca::verify_SUT_energy_balance(SUT_energy_balance = ".balanced") %>% 
    magrittr::extract2(".balanced") %>% 
    unlist() %>% 
    all()
}


#' Rename muscle work sectors to comport with IEA sectors.
#' 
#' The muscle work methodology in the `MWTools` package
#' uses slightly different final demand sector names compared to the IEA
#' and the `IEATools` package..
#' This function converts the `MWTools` sector names to appropriate IEA sector names.
#'
#' @param .df A data frame of muscle work data.
#' @param sector_colname The name of the sector column. 
#'                       Default is `MWTools::mw_constants$sector_col`.
#' @param original_sector_names A vector of string sector names that will be replaced.
#' @param new_sector_names A vector of string sector names that will appear in output.
#'
#' @return A data frame with renamed sectors.
#' 
#' @export
#'
#' @examples
#' df <- tibble::tribble(~Sector, ~value, 
#'                       MWTools::mw_sectors$transport_sector,         10, 
#'                       MWTools::mw_sectors$agriculture_broad.sector, 11,
#'                       MWTools::mw_sectors$services_broad.sector,    12,
#'                       MWTools::mw_sectors$industry_broad.sector,    13, 
#'                       "bogus",                                      14)
#' df
#' rename_mw_sectors(df)
rename_mw_sectors <- function(.df, 
                              sector_colname = MWTools::mw_constants$sector_col, 
                              original_sector_names = c(MWTools::mw_sectors$agriculture_broad.sector, 
                                                        MWTools::mw_sectors$transport_sector, 
                                                        MWTools::mw_sectors$services_broad.sector, 
                                                        MWTools::mw_sectors$industry_broad.sector), 
                              new_sector_names = c(IEATools::other_flows$agriculture_forestry, 
                                                   IEATools::transport_flows$transport_not_elsewhere_specified, 
                                                   IEATools::other_flows$commercial_and_public_services, 
                                                   IEATools::industry_flows$industry_not_elsewhere_specified)) {
  
  # Create a named vector to assist with refactoring.
  refactor_vector <- new_sector_names %>% 
    magrittr::set_names(original_sector_names)
  .df %>% 
    dplyr::mutate(
      "{sector_colname}" := dplyr::recode(.data[[sector_colname]], !!!refactor_vector)
    )
}


#' Load animal muscle work data
#' 
#' This function loads animal muscle work data and 
#' renames the sectors according to the default arguments to `rename_mw_sectors()`.
#'
#' @param fao_data_path The path to FAO data.
#' @param mw_concordance_path The path to the muscle work concordance.
#' @param amw_analysis_data_path The path the animal muscle work data.
#'
#' @return A data frame of animal muscle work data.
#' 
#' @export
load_amw_pfu_data <- function(fao_data_path, 
                              mw_concordance_path, 
                              amw_analysis_data_path) {
  readr::read_rds(file = fao_data_path) %>% 
    MWTools::calc_amw_pfu(concordance_path = mw_concordance_path,
                          amw_analysis_data_path = amw_analysis_data_path) %>% 
    rename_mw_sectors()
}


#' Load human muscle work data
#' 
#' This function loads human muscle work data and 
#' renames the sectors according to the default arguments to `rename_mw_sectors()`.
#'
#' @param ilo_data_path The path to ILO data.
#' @param mw_concordance_path The path to the muscle work concordance.
#' @param hmw_analysis_data_path The path the human muscle work data.
#'
#' @return A data frame of human muscle work data.
#' 
#' @export
load_hmw_pfu_data <- function(ilo_data_path, 
                              mw_concordance_path, 
                              hmw_analysis_data_path) {
  readr::read_rds(file = ilo_data_path) %>% 
    MWTools::calc_hmw_pfu(concordance_path = mw_concordance_path,
                          hmw_analysis_data_path = hmw_analysis_data_path) %>% 
    rename_mw_sectors()
}


