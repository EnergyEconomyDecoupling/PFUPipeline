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

