#' Title
#'
#' @param iea_psut An IEA PSUT data frame. 
#' @param mw_psut A muscle work PSUT data frame.
#' @param R The name of the column of `R` matrices. Default is `IEATools::psut_cols$R`.
#' @param U The name of the column of `U` matrices. Default is `IEATools::psut_cols$U`.
#' @param V The name of the column of `V` matrices. Default is `IEATools::psut_cols$V`.
#' @param Y The name of the column of `Y` matrices. Default is `IEATools::psut_cols$Y`.
#'
#' @return A data frame of summed matrices.
#' @export
#'
#' @examples
add_iea_mw_psut <- function(iea_psut, mw_psut, 
                            countries,
                            # Input columns
                            R = IEATools::psut_cols$R, 
                            U = IEATools::psut_cols$U,
                            V = IEATools::psut_cols$V,
                            Y = IEATools::psut_cols$Y,
                            U_feed = IEATools::psut_cols$U_feed,
                            U_eiou = IEATools::psut_cols$U_eiou,
                            # Metadata column names
                            country = IEATools::iea_cols$country,
                            year = IEATools::iea_cols$year,
                            method = IEATools::iea_cols$method,
                            energy_type = IEATools::iea_cols$energy_type,
                            last_stage = IEATools::iea_cols$last_stage,
                            # Output column names
                            r_eiou = IEATools::psut_cols$r_eiou) {

  # Define new column names.
  iea <- "_iea"
  R_iea <- paste0(R, iea)
  U_iea <- paste0(U, iea)
  U_feed_iea <- paste0(U_feed, iea)
  U_eiou_iea <- paste0(U_eiou, iea)
  V_iea <- paste0(V, iea)
  Y_iea <- paste0(Y, iea)
  mw <- "_mw"
  R_mw <- paste0(R, mw)
  U_mw <- paste0(U, mw)
  V_mw <- paste0(V, mw)
  Y_mw <- paste0(Y, mw)
  U_feed_mw <- paste0(U_feed, mw)
  U_eiou_mw <- paste0(U_eiou, mw)
  # Rename columns and delete the r_eiou column, as we will recalculate later.
  iea_specific <- iea_psut %>% 
    dplyr::filter(.data[[country]] %in% countries) %>% 
    dplyr::rename(
      "{R_iea}" := .data[[R]], 
      "{U_iea}" := .data[[U]], 
      "{V_iea}" := .data[[V]],
      "{Y_iea}" := .data[[Y]], 
      "{U_feed_iea}" := .data[[U_feed]],
      "{U_eiou_iea}" := .data[[U_eiou]]
    ) %>% 
    dplyr::mutate(
      "{r_eiou}" := NULL
    )
  mw_specific <- mw_psut %>% 
    dplyr::filter(.data[[country]] %in% countries) %>% 
    dplyr::rename(
      "{R_mw}" := .data[[R]], 
      "{U_mw}" := .data[[U]], 
      "{V_mw}" := .data[[V]],
      "{Y_mw}" := .data[[Y]],
      "{U_feed_mw}" := .data[[U_feed]],
      "{U_eiou_mw}" := .data[[U_eiou]]
    ) %>% 
    dplyr::mutate(
      "{r_eiou}" := NULL
    )
  
  # Join the data frames.
  dplyr::bind_cols(iea_specific, mw_specific, 
                   by = c(country, year, method, energy_type, last_stage)) %>% 
    dplyr::mutate(
      # Calculate new columns by summing matrices
      "{R}" := matsbyname::sum_byname(.data[[R_iea]], .data[[R_mw]])
    )
  
}