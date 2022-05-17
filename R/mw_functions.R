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