#' Calculate final-to-useful efficiencies
#' 
#' Knowing allocations (`C_mats`), machine efficiencies (`eta_m_vecs`), and
#' exergy-to-energy ratios (`phi_vecs`), it is possible to 
#' calculate the final-to-useful efficiencies for all 
#' final demand and energy industry own use
#' in an energy conversion chain. 
#' This function performs those calculations using
#' [Recca::calc_eta_fu_Y_eiou()].
#'
#' @param C_mats A data frame containing allocation matrices.
#' @param eta_fu_vecs A data frame containing vectors of machine efficiencies, probably the Etafuvecs target.
#' @param phi_vecs A data frame containing vectors of exergy-to-energy ratios, probably the Phivecs target.
#' @param countries The countries for which this analysis should be performed. 
#' @param country,last_stage,energy_type,method,year See `IEATools::iea_cols`.
#'
#' @return A data frame of final-to-useful efficiencies by energy sector and energy carrier.
#' 
#' @export
calc_fu_Y_EIOU_efficiencies <- function(C_mats, eta_fu_vecs, phi_vecs, countries, 
                                        country = IEATools::iea_cols$country,
                                        last_stage = IEATools::iea_cols$last_stage,
                                        energy_type = IEATools::iea_cols$energy_type,
                                        method = IEATools::iea_cols$method, 
                                        year = IEATools::iea_cols$year) {
  # Filter by desired country
  C_mats_filtered <- C_mats |> 
    dplyr::filter(.data[[country]] %in% countries)
  eta_m_vecs_filtered <- eta_fu_vecs |> 
    dplyr::filter(.data[[country]] %in% countries)
  phi_vecs_filtered <- phi_vecs |> 
    dplyr::filter(.data[[country]] %in% countries)
  
  # Make one large data frame. 
  df <- dplyr::full_join(C_mats_filtered, eta_m_vecs_filtered, by = c(country, last_stage, energy_type, method, year)) |> 
    dplyr::full_join(phi_vecs_filtered, by = c(country, year))
  
  df |> 
    Recca::calc_eta_fu_Y_eiou(eta_i = "eta.fu")
}