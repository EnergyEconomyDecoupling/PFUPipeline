#' Information about the machine efficiency files
#'
#' A string list containing information about machine efficiency files.
#' Items in the list provide default values for machine efficiency files,
#' including Excel tab names, etc.
#'
#' @format A string list with `r length(machine_constants)` entries.
#' \describe{
#' \item{efficiency_tab_name}{The default name of the efficiency tabs in machine efficiency excel files.}
#' }
#'
#' @examples
#' machine_constants
"machine_constants"


#' Socioeconomic data column names
#'
#' A string list containing values for the column names of socioeconomic data. See `pwt10::pwt10.0`.
#'
#' @format A string list with `r length(socioecon_cols)` entries.
#' \describe{
#' \item{isocode_colname}{The name of a metadata column containing the 3-letter isocodes of countries.}
#' \item{year_colname}{The name of a metadata column containing values for the year of the observation. Lower case.}
#' \item{rgdpe_colname}{The name of the column containing data for Expenditure-side real GDP at chained PPPs (in million 2017 USD).}
#' \item{rgdpo_colname}{The name of the column containing data for Output-side real GDP at chained PPPs (in million 2017 USD).}
#' \item{rgdpna_colname}{The name of the column containing data for Real GDP at constant 2017 national prices (in million 2017 USD).}
#' \item{emp_colname}{The name of the column containing data for Number of persons engaged (in millions).}
#' \item{avh_colname}{The name of the column containing data for Average annual hours worked by persons engaged.}
#' \item{hc_colname}{The name of the column containing data for Human capital index, based on years of schooling and returns to education; see Human capital in PWT9.}
#' \item{rnna_colname}{The name of the column containing data for Capital stock at constant 2017 national prices (in million 2017 USD).}
#' \item{rkna_colname}{The name of the column containing data for Capital services at constant 2017 national prices (2017 = 1).}
#' \item{K_colname}{A more representative name for `rnna_colname`.}
#' \item{Kserv_colname}{A more representative name for `rkna_colname`.}
#' \item{L_colname}{The name of the column containing data for the total number of hours worked in a given year. See `PFUWorkflow::get_L_K_GDP_data`.}
#' \item{Ladj_colname}{The name of the column containing data for the total number of hours worked adjusted by the human capital index. See `PFUWorkflow::get_L_K_GDP_data`.}
#' }
#'
#' @examples
#' socioecon_cols
"socioecon_cols"