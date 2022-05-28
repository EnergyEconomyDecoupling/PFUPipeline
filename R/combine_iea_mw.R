#' Sum IEA and muscle work ECC matrices
#' 
#' To create a combined energy conversion chain (ECC)
#' containing both IEA and muscle work data, 
#' PSUT matrices for each ECC are summed.
#' This function sums `R`, `U`, `V`, `Y`, `U_feed`, and `U_eiou` matrices directly.
#' It also re-calculates the `r_eiou` matrix.
#' 
#' If either of `iea_psut` or `.mw_psut` are `NULL`, 
#' the other is returned.
#'
#' @param iea_psut An IEA PSUT data frame. 
#' @param mw_psut A muscle work PSUT data frame.
#' @param R The name of the column of `R` matrices. Default is `IEATools::psut_cols$R`.
#' @param U The name of the column of `U` matrices. Default is `IEATools::psut_cols$U`.
#' @param V The name of the column of `V` matrices. Default is `IEATools::psut_cols$V`.
#' @param Y The name of the column of `Y` matrices. Default is `IEATools::psut_cols$Y`.
#'
#' @return A data frame of summed matrices.
#' 
#' @export
add_iea_mw_psut <- function(.iea_psut, .mw_psut, 
                            countries,
                            # Input columns
                            R = IEATools::psut_cols$R, 
                            U = IEATools::psut_cols$U,
                            V = IEATools::psut_cols$V,
                            Y = IEATools::psut_cols$Y,
                            U_feed = IEATools::psut_cols$U_feed,
                            U_eiou = IEATools::psut_cols$U_eiou,
                            s_units = IEATools::psut_cols$s_units,
                            # Metadata column names
                            country = IEATools::iea_cols$country,
                            year = IEATools::iea_cols$year,
                            method = IEATools::iea_cols$method,
                            energy_type = IEATools::iea_cols$energy_type,
                            last_stage = IEATools::iea_cols$last_stage,
                            # Output column names
                            r_eiou = IEATools::psut_cols$r_eiou) {

  if (is.null(.mw_psut)) {
    return(.iea_psut)
  }
  if (is.null(.iea_psut)) {
    return(.mw_psut)
  }
  # Define new column names.
  iea <- "_iea"
  R_iea <- paste0(R, iea)
  U_iea <- paste0(U, iea)
  U_feed_iea <- paste0(U_feed, iea)
  U_eiou_iea <- paste0(U_eiou, iea)
  V_iea <- paste0(V, iea)
  Y_iea <- paste0(Y, iea)
  s_units_iea <- paste0(s_units, iea)
  mw <- "_mw"
  R_mw <- paste0(R, mw)
  U_mw <- paste0(U, mw)
  V_mw <- paste0(V, mw)
  Y_mw <- paste0(Y, mw)
  U_feed_mw <- paste0(U_feed, mw)
  U_eiou_mw <- paste0(U_eiou, mw)
  s_units_mw <- paste0(s_units, mw)
  
  # Rename columns and delete the r_eiou column, as we will recalculate later.
  iea_specific <- .iea_psut %>% 
    dplyr::filter(.data[[country]] %in% countries) %>% 
    dplyr::rename(
      "{R_iea}" := .data[[R]], 
      "{U_iea}" := .data[[U]], 
      "{V_iea}" := .data[[V]],
      "{Y_iea}" := .data[[Y]], 
      "{U_feed_iea}" := .data[[U_feed]],
      "{U_eiou_iea}" := .data[[U_eiou]], 
      "{s_units_iea}" := .data[[s_units]]
    ) %>% 
    dplyr::mutate(
      "{r_eiou}" := NULL
    )
  mw_specific <- .mw_psut %>% 
    dplyr::filter(.data[[country]] %in% countries) %>% 
    dplyr::rename(
      "{R_mw}" := .data[[R]], 
      "{U_mw}" := .data[[U]], 
      "{V_mw}" := .data[[V]],
      "{Y_mw}" := .data[[Y]],
      "{U_feed_mw}" := .data[[U_feed]],
      "{U_eiou_mw}" := .data[[U_eiou]], 
      "{s_units_mw}" := .data[[s_units]]
    ) %>% 
    dplyr::mutate(
      "{r_eiou}" := NULL
    )
  
  # Join the data frames.
  dplyr::full_join(iea_specific, mw_specific, 
                   by = c(country, year, method, energy_type, last_stage)) %>% 
    dplyr::mutate(
      # Calculate new columns by summing matrices
      "{R}" := matsbyname::sum_byname(.data[[R_iea]], .data[[R_mw]]), 
      "{U}" := matsbyname::sum_byname(.data[[U_iea]], .data[[U_mw]]), 
      "{V}" := matsbyname::sum_byname(.data[[V_iea]], .data[[V_mw]]), 
      "{Y}" := matsbyname::sum_byname(.data[[Y_iea]], .data[[Y_mw]]), 
      "{U_feed}" := matsbyname::sum_byname(.data[[U_feed_iea]], .data[[U_feed_mw]]), 
      "{U_eiou}" := matsbyname::sum_byname(.data[[U_eiou_iea]], .data[[U_eiou_mw]]), 
      "{s_units}" := matsbyname::sum_byname(.data[[s_units_iea]], .data[[s_units_mw]]), 
      "{r_eiou}" := matsbyname::quotient_byname(.data[[U_eiou]], .data[[U]]) %>% 
        # For cases where U is 0, will get 0/0 = NaN.  Convert to zero.
        matsbyname::replaceNaN_byname(val = 0), 
      # Delete unneeded columns
      "{R_iea}" := NULL,
      "{R_mw}" := NULL,
      "{U_iea}" := NULL,
      "{U_mw}" := NULL,
      "{V_iea}" := NULL,
      "{V_mw}" := NULL,
      "{Y_iea}" := NULL,
      "{Y_mw}" := NULL,
      "{U_feed_iea}" := NULL,
      "{U_feed_mw}" := NULL,
      "{U_eiou_iea}" := NULL,
      "{U_eiou_mw}" := NULL,
      "{s_units_iea}" := NULL,
      "{s_units_mw}" := NULL
    )
}