#' Read and clean IEA data
#' 
#' Loads the OECD and non-OECD data files and makes a single data frame.
#' Internally, this function uses IEATools::load_tidy_iea_df().
#'
#' @param oecd_path the path to the OECD data file
#' @param nonoecd_path the path to the non-OECD data file
#'
#' @return a single data frame containing all IEA data
#' 
#' @export
#'
#' @examples
#' paths <- get_paths()
#' load_all_IEA_data(paths$oecd_path, paths$nonoecd_path)
load_IEA_data <- function(oecd_path, nonoecd_path) {
  oecd <- load_tidy_iea_df(oecd_path)
  # nonoecd <- IEATools::load_tidy_iea_df(nonoecd_path)
  # dplyr::bind_rows(oecd, nonoecd)
}

#' Extract specific country data from AllIEAData
#' 
#' Data is extracted according to the `countries` object.
#' ``dplyr::filter()`` does the subsetting.
#'
#' @param AllIEAData a data frame containing cleaned IEA extended energy balance data
#' @param countries a list of 3-letter country codes for countries to be analyzed
#'
#' @return a data frame with the desired IEA data only
#' 
#' @export
extract_country_data <- function(AllIEAData, countries) {
  filter(AllIEAData, Country %in% countries)
}