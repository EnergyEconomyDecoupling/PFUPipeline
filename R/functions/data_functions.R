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


#' Tells whether IEA data are balanced
#' 
#' Performs the check in a way that is amenable to subtargets in drake.
#' Internally, this function uses [IEATools::calc_tidy_iea_df_balances()].
#' Grouping is doing internal to this function using the value of `grp_vars`.
#'
#' @param IEAData a tidy IEA data frame
#' @param countries the countries for which balancing should be checked as strings
#' @param grp_vars the groups that should be checked.  Default is `c("Country", "Method", "Energy.type", "Last.stage", "Product")`.
#'
#' @return a logical stating whether all products are balanced for the country of interest
#' 
#' @export
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


#' Create a final-to-useful allocation template
#'
#' @param country a string of the 3-letter ISO country code
#' @param file_name the file name for the template. Default is "FU Allocations <<3-letter country code>>".
#' @param ext the file name extension. Default is ".xlsx".
#' @param data_target the data target from which the template is created. Default is "Specified".
#' @param paths_name the name of the paths object. Default is "paths".
#' @param fu_analysis_path_name the name of the final-to-useful path member of the paths object. Default is "fu_analysis_path".
#'
#' @return the path to the final-to-useful analysis template
#' 
#' @export
generate_allocation_template <- function(country,
                                         file_name = paste0("FU Allocations ", country),
                                         ext = ".xlsx",
                                         data_target = "Specified",
                                         paths_name = "paths",
                                         fu_analysis_path_name = "fu_analysis_path") {
  output_path <- file.path(readd(paths_name, character_only = TRUE)[[fu_analysis_path_name]], 
                           paste0(file_name, ext))
  # Get the specified data for this country
  readd_by_country(data_target, country) %>%
    # Create the allocation template
    fu_allocation_template() %>%
    # Write the allocation template
    write_fu_allocation_template(output_path)
}


generate_eta_fu_template <- function(country) {
  
}