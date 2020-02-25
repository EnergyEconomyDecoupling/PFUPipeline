#' Extract specific country data from AllIEAData
#' 
#' Data is extracted according to the `countries` object.
#' [dplyr::filter()] does the subsetting.
#'
#' @param AllIEAData a data frame containing cleaned IEA extended energy balance data
#' @param countries a list of 3-letter country codes for countries to be analyzed
#'
#' @return a data frame with the desired IEA data only
#' 
#' @export
extract_country_data <- function(AllIEAData, countries, max_year) {
  filter(AllIEAData, Country %in% countries, Year <= max_year)
}

is_balanced <- function(IEAData, countries, grp_vars = c("Country", "Method", "Energy.type", "Last.stage", "Product")) {
  filter(IEAData, Country %in% countries) %>% 
    group_by(!!as.name(grp_vars)) %>%  
    calc_tidy_iea_df_balances() %>% 
    tidy_iea_df_balanced()
}

make_balanced <- function(IEAData, countries, grp_vars = c("Country", "Method", "Energy.type", "Last.stage", "Product")) {
  filter(IEAData, Country %in% countries) %>% 
    group_by(!!as.name(grp_vars)) %>%  
    fix_tidy_iea_df_balances() %>% 
    ungroup()
}

specify <- function(BalancedIEAData, countries) {
  filter(BalancedIEAData, Country %in% countries) %>% 
    specify_all()
}

make_psut <- function(SpecifiedIEAData, countries) {
  filter(SpecifiedIEAData, Country %in% countries) %>% 
    prep_psut()
}

#' Read a subtarget based on country
#'
#' @param target the name of the drake target as a string
#' @param country the 3-letter ISO abbreviation of the country for whom `target` is to be readd from the drake cache, as a string
#' @param name_of_countries_object a string giving the name of the countries object. Default is "countries".
#'
#' @return the country-specific version of `target`
#' 
#' @export
readd_by_country <- function(target, country, name_of_countries_object = "countries") {
  country_index <- which(country == readd(name_of_countries_object, character_only = TRUE), arr.ind = TRUE)
  readd(target, character_only = TRUE, subtargets = country_index)
}

generate_allocation_template <- function(country) {
  
}

generate_eta_fu_template <- function(country) {
  
}