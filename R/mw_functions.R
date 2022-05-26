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
  .psut_df %>% 
    dplyr::filter(.data[[country]] %in% countries) %>% 
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
  
  
}