#' Get all pwt10 data for a set of countries
#'
#' Using the pwt10 package this function creates a data frame containing all the
#' data Penn World Tables 10 (pwt10) for a set of countries as specified in a string
#' of 3-letter ISO country codes named `countries`.
#' Note that some data is not available for some countries (mostly non-OECD).
#'
#' @param countries A string of iso 3-letter country codes
#' @param isocode_colname See `PFUWorkflow::socioecon_cols`.
#'
#' @return A data frame containing socioeconomic data from pwt10 for a set of countries.

#' @export
#'
#' @examples
#' countries <- c("GBR")
#' all_pwt_data <- get_all_pwt_data(countries = countries)
get_all_pwt_data <- function(countries,
                             isocode_colname = PFUDatabase::socioecon_cols$isocode_colname) {
  
  # Get all pwt10 data and filter for countries in the string countries
  pwt10_data <- pwt10::pwt10.0 %>%
    dplyr::filter(.data[[isocode_colname]] %in% countries)
  
  # Remove rownames
  rownames(pwt10_data) <- NULL
  
  return(pwt10_data)
}


#' Create a dataframe containing capital (K), labor (L), and GDP data
#'
#' This function selects the following columns from a pwt10 data frame produced
#' by calling `pwt10::pwt10.0`,
#' with descriptions from pwt10 documentation:
#'   isocode: 3-letter isocode
#'   year: Year
#'   rgdpe: Expenditure-side real GDP at chained PPPs (in million 2017 USD).
#'   rgdpo: Output-side real GDP at chained PPPs (in million 2017 USD).
#'   rgdpna: Real GDP at constant 2017 national prices (in million 2017 USD)
#'   emp: Number of persons engaged (in millions)
#'   avh: Average annual hours worked by persons engaged.
#'   hc: Human capital index, based on years of schooling and returns to education;
#'       see Human capital in PWT9.
#'   rnna: Capital stock at constant 2017 national prices (in million 2017 USD).
#'   rkna: Capital services at constant 2017 national prices (2017 = 1).
#'
#' The metrics  L, the total number of hours worked in a given year
#' and Ladj, the number of hours worked adjusted by the human capital index are
#' also calculated and added as columns, with avh, hc, and emp being removed after use.
#'
#' Note that some data is not available for some countries (mostly non-OECD),
#' some of the calculated metrics i.e. Adjusted Labor (L.adj) is also absent.
#'
#' @param pwt10_data A data frame containing all pwt10 data for at least one country,
#'                   usually obtained from `get_all_pwt_data()`.
#' @param isocode_colname,year_colname,rgdpe_colname,rgdpo_colname,rgdpna_colname,emp_colname,avh_colname,hc_colname,rnna_colname,rkna_colname,K_colname,Kserv_colname,L_colname,Ladj_colname See `PFUDatabase::socioecon_cols`.
#' @param country_colname,Year_colname See `IEATools::iea_cols`.
#'
#' @return A data frame containing three GDP metrics, Labor, Adjusted Labor,
#'         Capital, and Capital services.
#' @export
#'
#' @examples
#' countries <- c("GBR")
#' L_K_GDP_data <- get_all_pwt_data(countries = countries) %>%
#'                   get_L_K_GDP_data()
get_L_K_GDP_data <- function(pwt10_data,
                             country_colname = IEATools::iea_cols$country,
                             Year_colname= IEATools::iea_cols$year,
                             year_colname= PFUDatabase::socioecon_cols$year_colname,
                             isocode_colname = PFUDatabase::socioecon_cols$isocode_colname,
                             rgdpe_colname = PFUDatabase::socioecon_cols$rgdpe_colname,
                             rgdpo_colname = PFUDatabase::socioecon_cols$rgdpo_colname,
                             rgdpna_colname = PFUDatabase::socioecon_cols$rgdpna_colname,
                             emp_colname = PFUDatabase::socioecon_cols$emp_colname,
                             avh_colname = PFUDatabase::socioecon_cols$avh_colname,
                             hc_colname = PFUDatabase::socioecon_cols$hc_colname,
                             rnna_colname = PFUDatabase::socioecon_cols$rnna_colname,
                             rkna_colname = PFUDatabase::socioecon_cols$rkna_colname,
                             K_colname = PFUDatabase::socioecon_cols$K_colname,
                             Kserv_colname = PFUDatabase::socioecon_cols$Kserv_colname,
                             L_colname = PFUDatabase::socioecon_cols$L_colname,
                             Ladj_colname = PFUDatabase::socioecon_cols$Ladj_colname) {
  
  # Select columns
  L_K_GDP_data <- pwt10_data %>%
    dplyr::select(.data[[isocode_colname]], .data[[year_colname]], .data[[rgdpe_colname]],
                  .data[[rgdpo_colname]], .data[[rgdpna_colname]], .data[[emp_colname]],
                  .data[[avh_colname]], .data[[hc_colname]], .data[[rnna_colname]], .data[[rkna_colname]])
  
  # Rename columns
  L_K_GDP_data <- L_K_GDP_data %>%
    magrittr::set_colnames(c(country_colname, Year_colname, rgdpe_colname,
                             rgdpo_colname, rgdpna_colname, emp_colname,
                             avh_colname, hc_colname, K_colname, Kserv_colname))
  
  # Calculate L, the total number of hours worked in a given year
  # and Ladj, the total number of hours worked adjusted by the human capital index
  L_K_GDP_data %>%
    dplyr::mutate(
      "{L_colname}" := (.data[[emp_colname]] * .data[[avh_colname]] * 1000000), .keep = "unused", .after = .data[[rgdpna_colname]]
    ) %>% 
    dplyr::mutate(
      "{Ladj_colname}" := (.data[[L_colname]] * .data[[hc_colname]]), .after = .data[[L_colname]]
    ) %>%
    dplyr::select(-.data[[hc_colname]])
}
