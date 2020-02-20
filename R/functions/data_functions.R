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