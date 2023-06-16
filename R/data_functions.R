
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
#' Data is extracted according to the `countries` and `years` objects
#' in a way that is amenable to drake subtargets.
#' `dplyr::filter()` does the subsetting.
#'
#' @param .df A data frame containing cleaned data with lots of countries and years.
#' @param countries A list of country codes for countries to be analyzed. 
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
# filter_countries_years <- function(.df, 
#                                    countries, 
#                                    years,
#                                    country = IEATools::iea_cols$country,
#                                    year = IEATools::iea_cols$year) {
#   countries1 <- length(countries) == 1
#   years1 <- length(years) == 1
#   if (countries1 & years1) {
#       if (countries == "all" & years == "all") {
#         return(.df)
#       }
#   }
#   if (countries1) {
#     if (countries == "all") {
#       return(.df %>% dplyr::filter(.data[[year]] %in% years))
#     }
#   }
#   if (years1) {
#     if (years == "all") {
#       return(.df %>% dplyr::filter(.data[[country]] %in% countries))
#     }
#   }
#   .df %>% 
#     dplyr::filter(.data[[country]] %in% countries, .data[[year]] %in% years)
# }


#' Tells whether IEA data are balanced
#'
#' Performs the the energy balance check in a way that is amenable to drake subtargets.
#' Internally, this function uses [IEATools::calc_tidy_iea_df_balances()].
#' Grouping is doing internal to this function using the value of `grp_vars`.
#'
#' @param .iea_data a tidy IEA data frame
#' @param countries the countries for which balancing should be checked as strings
#' @param country The name of the country column in `IEAData`. Default is `r IEATools::iea_cols$country`.
#' @param grp_vars the groups that should be checked. Default is
#'                 `c(country, IEATools::iea_cols$method, IEATools::iea_cols$energy_type, IEATools::iea_cols$last_stage, IEATools::iea_cols$product)`.
#'
#' @return a logical stating whether all products are balanced for the country of interest
#'
#' @export
#'
#' @examples
#' # These data are not balanced, because they are raw.
#' IEATools::sample_iea_data_path() %>%
#'   IEATools::load_tidy_iea_df() %>%
#'   is_balanced(countries = "ZAF")
is_balanced <- function(.iea_data, 
                        countries,
                        country = IEATools::iea_cols$country,
                        grp_vars = c(country,
                                     IEATools::iea_cols$method,
                                     IEATools::iea_cols$energy_type,
                                     IEATools::iea_cols$last_stage,
                                     IEATools::iea_cols$year,
                                     IEATools::iea_cols$product)) {
  dplyr::filter(.iea_data, .data[[country]] %in% countries) %>%
    dplyr::group_by(!!as.name(grp_vars)) %>%
    IEATools::calc_tidy_iea_df_balances() %>%
    IEATools::tidy_iea_df_balanced()
}


#' Balance IEA data
#'
#' Balances the IEA data in a way that is amenable to drake subtargets.
#' Internally, this function uses `IEATools::fix_tidy_iea_df_balances()`.
#' Grouping is done internal to this function using the value of `grp_vars`.
#'
#' @param .iea_data A tidy IEA data frame.
#' @param max_fix The maximum allowable energy imbalance to fix.
#'                Default is `3`.
#' @param countries The countries that should be balanced.
#' @param grp_vars the groups that should be checked. 
#'                 Default is
#'                 `c(country, IEATools::iea_cols$method, IEATools::iea_cols$energy_type, IEATools::iea_cols$last_stage, IEATools::iea_cols$product)`.
#' @param country See `IEATools::iea_cols`
#'
#' @return A data frame of balanced IEA data.
#'
#' @export
#'
#' @examples
#' IEATools::sample_iea_data_path() %>%
#'   IEATools::load_tidy_iea_df() %>%
#'   make_balanced(countries = c("GHA", "ZAF")) %>%
#'   is_balanced(countries = c("GHA", "ZAF"))
make_balanced <- function(.iea_data,
                          max_fix = 6,
                          countries,
                          country = IEATools::iea_cols$country,
                          grp_vars = c(country,
                                       IEATools::iea_cols$method,
                                       IEATools::iea_cols$energy_type,
                                       IEATools::iea_cols$last_stage,
                                       IEATools::iea_cols$year,
                                       IEATools::iea_cols$product)) {
  dplyr::filter(.iea_data, .data[[country]] %in% countries) %>%
    dplyr::group_by(!!as.name(grp_vars)) %>%
    IEATools::fix_tidy_iea_df_balances(max_fix = max_fix) %>%
    dplyr::ungroup()
}


#' Specify the IEA data
#'
#' Specifies the IEA data in a way that is amenable to drake subtargets.
#' See `IEATools::specify_all()` for details.
#'
#' @param BalancedIEAData IEA data that have already been balanced
#' @param countries the countries for which specification should occur
#' @param country See `IEATools::iea_cols`.
#'
#' @return a data frame of specified IEA data
#'
#' @export
#'
#' @examples
#' IEATools::sample_iea_data_path() %>%
#'   IEATools::load_tidy_iea_df() %>%
#'   make_balanced(countries = c("GHA", "ZAF")) %>%
#'   specify(countries = c("GHA", "ZAF"))
specify <- function(BalancedIEAData,
                    countries,
                    country = IEATools::iea_cols$country) {
  dplyr::filter(BalancedIEAData, .data[[country]] %in% countries) %>%
    IEATools::specify_all()
}


#' Convert to PSUT matrices
#'
#' Converts tidy IEA data to PSUT matrices in a way that is amenable to drake subtargets.
#' Internally, `IEATools::prep_psut()` does the conversion to matrices.
#'
#' @param SpecifiedIEAData A data frame that has already been specified via `specify()`.
#' @param countries The countries you want to convert to PSUT matrices.
#' @param matrix_class The type of matrix to be created. 
#'                     One of "matrix" (the base class) or 
#'                     "Matrix" (for sparse matrices).
#' @param country See `IEATools::iea_cols`.
#'
#' @return A `matsindf`-style data frame.
#'
#' @export
#'
#' @examples
#' IEATools::sample_iea_data_path() %>%
#'   IEATools::load_tidy_iea_df() %>%
#'   make_balanced(countries = c("GHA", "ZAF")) %>%
#'   specify(countries = c("GHA", "ZAF")) %>%
#'   make_iea_psut(countries = c("GHA", "ZAF"))
make_iea_psut <- function(SpecifiedIEAData,
                          countries,
                          matrix_class = c("matrix", "Matrix"),
                          country = IEATools::iea_cols$country) {
  matrix_class <- match.arg(matrix_class)
  SpecifiedIEAData |> 
    dplyr::filter(.data[[country]] %in% countries) |> 
    IEATools::prep_psut(matrix_class = matrix_class)
}
