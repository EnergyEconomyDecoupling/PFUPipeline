
#' Load a country concordance table
#'
#' In addition to loading the country concordance table,
#' this function checks for errors:
#' missing 3-letter country codes and repeated 3-letter country codes.
#' Both of these issues will cause a problem.
#'
#' @param country_concordance_path The path to the country concordance file.
#'                                 The file is assumed to be an Excel file.
#' @param sheet The name of the sheet to be read.
#'              Default is "country_concordance_table".
#' @param pfu_code_colname The name of the column in the country concordance table
#'                         that contains 3-letter country codes
#'                         to be used in the workflow.
#'                         Default is "PFU.code".
#'
#' @return A country concordance table.
#'
#' @export
load_country_concordance_table <- function(country_concordance_path,
                                           sheet = "country_concordance_table",
                                           pfu_code_colname = "PFU.code") {
  out <- readxl::read_excel(country_concordance_path, sheet = "country_concordance_table")
  # Ensure there are no duplicated PFU.codes
  # (3-letter country codes that we'll use in the workflow).
  country_codes <- out[[pfu_code_colname]]
  # Ensure no NA's in PFU.code column
  assertthat::assert_that(! any(is.na(country_codes)),
                          msg = paste0("Found empty values in column ",
                                       pfu_code_colname,
                                       " in the country concordance table."))
  # Ensure no repeated values in the PFU.code column.
  assertthat::assert_that(length(country_codes) == length(unique(country_codes)),
                          msg = paste0("Found repeated country codes in the ",
                                       pfu_code_colname,
                                       " column of the country concordance table."))
  return(out)
}


#' Extract specific country and year data
#'
#' Data is extracted according to the `countries` oand `years` objects
#' in a way that is amenable to drake subtargets.
#' `dplyr::filter()` does the subsetting.
#'
#' @param .df A data frame containing cleaned data with lots of countries and years.
#' @param countries A list of 3- or 4-letter country codes for countries to be analyzed. 
#'                  "all" means return all countries.
#' @param years A vector of years. "all" means return all years. 
#' @param country,year See `IEATools::iea_cols`.
#'
#' @return A data frame with the desired IEA data only.
#'
#' @export
#'
#' @examples
#' IEATools::sample_iea_data_path() %>%
#'   IEATools::load_tidy_iea_df() %>%
#'   filter_countries_years(countries = c("ZAF"), years = 1960:1999)
filter_countries_years <- function(.df, 
                                   countries, 
                                   years,
                                   country = IEATools::iea_cols$country,
                                   year = IEATools::iea_cols$year) {
  countries1 <- length(countries) == 1
  years1 <- length(years) == 1
  if (countries1 & years1) {
      if (countries == "all" & years == "all") {
        return(.df)
      }
  }
  if (countries1) {
    if (countries == "all") {
      return(.df %>% dplyr::filter(.data[[year]] %in% years))
    }
  }
  if (years1) {
    if (years == "all") {
      return(.df %>% dplyr::filter(.data[[country]] %in% countries))
    }
  }
  .df %>% 
    dplyr::filter(.data[[country]] %in% countries, .data[[year]] %in% years)
}
