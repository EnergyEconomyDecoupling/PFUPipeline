#' #' Extract specific country data from AllIEAData
#' #' 
#' #' Data is extracted according to the `countries` object in a way that is amenable to drake subtargets.
#' #' [dplyr::filter()] does the subsetting.
#' #'
#' #' @param AllIEAData a data frame containing cleaned IEA extended energy balance data
#' #' @param countries a list of 3-letter country codes for countries to be analyzed
#' #'
#' #' @return a data frame with the desired IEA data only
#' #' 
#' #' @export
#' extract_country_data <- function(AllIEAData, countries, max_year) {
#'   out <- filter(AllIEAData, Country %in% countries, Year <= max_year)
#'   assertthat::validate_that(nrow(out) != 0, msg = paste0("No rows of data for countries ", countries))
#'   return(out)
#' }
#' 
#' 
#' #' Tells whether IEA data are balanced
#' #' 
#' #' Performs the the energy balance check in a way that is amenable to drake subtargets.
#' #' Internally, this function uses [IEATools::calc_tidy_iea_df_balances()].
#' #' Grouping is doing internal to this function using the value of `grp_vars`.
#' #'
#' #' @param IEAData a tidy IEA data frame
#' #' @param countries the countries for which balancing should be checked as strings
#' #' @param grp_vars the groups that should be checked.  Default is `c("Country", "Method", "Energy.type", "Last.stage", "Product")`.
#' #'
#' #' @return a logical stating whether all products are balanced for the country of interest
#' #' 
#' #' @export
#' is_balanced <- function(IEAData, countries, grp_vars = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Product")) {
#'   filter(IEAData, Country %in% countries) %>% 
#'     group_by(!!as.name(grp_vars)) %>%  
#'     calc_tidy_iea_df_balances() %>% 
#'     tidy_iea_df_balanced()
#' }
#' 
#' 
#' #' Balance IEA data
#' #' 
#' #' Balances the IEA data in a way that is amenable to drake subtargets.
#' #' Internally, this function uses `IEATools::fix_tidy_iea_df_balances()`.
#' #' Grouping is doing internal to this function using the value of `grp_vars`.
#' #'
#' #' @param IEAData a tidy IEA data frame
#' #' @param countries the countries that should be balanced
#' #' @param grp_vars the groups that should be checked for energy balance.
#' #'                 Default is `c("Country", "Method", "Energy.type", "Last.stage", "Product")`.
#' #'
#' #' @return balanced IEA data
#' #' 
#' #' @export
#' make_balanced <- function(IEAData, countries, grp_vars = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Product")) {
#'   filter(IEAData, Country %in% countries) %>% 
#'     group_by(!!as.name(grp_vars)) %>%  
#'     fix_tidy_iea_df_balances() %>% 
#'     ungroup()
#' }
#' 
#' 
#' #' Specify the IEA data
#' #' 
#' #' Specifies the IEA data in a way that is amenable to drake subtargets.
#' #' See [IEATools::specify_all()] for details.
#' #'
#' #' @param BalancedIEAData IEA data that have already been balanced
#' #' @param countries the countries for which specificaion should occur
#' #'
#' #' @return a data frame of specified IEA data
#' #' 
#' #' @export
#' specify <- function(BalancedIEAData, countries) {
#'   filter(BalancedIEAData, Country %in% countries) %>% 
#'     specify_all()
#' }
#' 
#' 
#' #' Convert to PSUT matrices
#' #' 
#' #' Converts tidy IEA data to PSUT matrices in a way that is amenable to drake subtargets.
#' #' Internally, [IEATools::prep_psut()] does the conversion to matrices.
#' #'
#' #' @param SpecifiedIEAData 
#' #' @param countries 
#' #'
#' #' @return a [matsindf]-style data frame
#' #' @export
#' make_psut <- function(SpecifiedIEAData, countries) {
#'   filter(SpecifiedIEAData, Country %in% countries) %>% 
#'     prep_psut()
#' }