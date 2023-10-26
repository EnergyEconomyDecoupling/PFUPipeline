#' Calculate final-to-useful efficiencies
#' 
#' Knowing allocations (`C_mats`), machine efficiencies (`eta_m_vecs`), and
#' exergy-to-energy ratios (`phi_vecs`), it is possible to 
#' calculate the final-to-useful efficiencies for all 
#' final demand and energy industry own use
#' in an energy conversion chain. 
#' This function performs those calculations using
#' `Recca::calc_eta_fu_Y_eiou()`.
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



#' Calculates the aggregated C matrices
#'
#' @param C_mats A data frame containing the C matrices.
#' @param psut_iea A data frame containing IEA data at the final stage, in energy terms, in a PSUT format.
#' @param C_EIOU The name of the column containing the C_EIOU matrix in the input data frame.
#' @param C_Y The name of the column containing the C_Y matrix in the input data frame.
#' @param Y The name of the column containing the Y matrix in the input data frame.
#' @param U_EIOU The name of the column containing the U_EIOU matrix in the input data frame.
#' @param C_EIOU_agg The name of the column containing the C_EIOU aggregated matrix in the output data frame.
#' @param C_Y_agg The name of the column containing the C_Y aggregated matrix in the output data frame.
#' @param C_EIOU_Y_agg The name of the column containing the C_EIOU_Y aggregated matrix in the output data frame.
#' @param country,method,energy_type,last_stage,year See `IEATools::iea_cols`.
#'
#' @return A data frame containing the aggregated C matrices for EIOU, Y and EIOU and Y together.
#' @export
calc_C_mats_agg <- function(C_mats,
                            psut_iea,
                            C_EIOU = "C_EIOU",
                            C_Y = "C_Y",
                            Y = "Y",
                            U_EIOU = "U_EIOU",
                            C_EIOU_agg = "C_EIOU_agg",
                            C_Y_agg = "C_Y_agg",
                            C_EIOU_Y_agg = "C_EIOU_Y_agg",
                            country = IEATools::iea_cols$country,
                            method = IEATools::iea_cols$method,
                            energy_type = IEATools::iea_cols$energy_type,
                            last_stage = IEATools::iea_cols$last_stage,
                            year = IEATools::iea_cols$year){
  
  C_mats_agg <- dplyr::left_join(
    C_mats, psut_iea, by = c({country}, {method}, {energy_type}, {last_stage}, {year})
  ) |> 
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[C_EIOU]], .data[[C_Y]], .data[[Y]], .data[[U_EIOU]]) |> 
    # Calculating all the bunch of vectors and matrices needed now:
    dplyr::mutate(
      # Total use of product p industry EIOU industry i, as a vector. U_EIOU vectorised.
      eiou_vec = matsbyname::vectorize_byname(a = .data[[U_EIOU]], notation = list(RCLabels::arrow_notation)),
      # Total use of product p in final demand sector s, as a vector. Y vectorised.
      y_vec = matsbyname::vectorize_byname(a = .data[[Y]], notation = list(RCLabels::arrow_notation)),
      # Total use of product p in machine m across all EIOU industries aggregated
      Alloc_mat_EIOU = matsbyname::matrixproduct_byname(matsbyname::hatize_byname(eiou_vec),
                                                        .data[[C_EIOU]]) |> 
        matsbyname::aggregate_pieces_byname(piece = "noun", 
                                            margin = 1, 
                                            notation = list(RCLabels::arrow_notation)),
      # Total use of product p in final demand sector s across all final demand sectors aggregated
      Alloc_mat_Y = matsbyname::matrixproduct_byname(matsbyname::hatize_byname(y_vec),
                                                     .data[[C_Y]]) |> 
        matsbyname::aggregate_pieces_byname(piece = "noun", 
                                            margin = 1, 
                                            notation = list(RCLabels::arrow_notation)),
      # Total use of product p in machine m Y and EIOU aggregated
      Alloc_mat_EIOU_Y = matsbyname::sum_byname(Alloc_mat_EIOU, Alloc_mat_Y),
      # Total use of product p in EIOU industries
      f_EIOU = matsbyname::rowsums_byname(Alloc_mat_EIOU),
      # Total use of product p in Y
      f_Y = matsbyname::rowsums_byname(Alloc_mat_Y),
      # Total use of product p in EIOU and Y
      f_EIOU_Y = matsbyname::rowsums_byname(Alloc_mat_EIOU_Y),
      # Share of product p used in each machine m across EIOU. Sum by product yields 1.
      "{C_EIOU_agg}" := matsbyname::matrixproduct_byname(matsbyname::hatinv_byname(f_EIOU, keep = "rownames"),
                                                         Alloc_mat_EIOU),
      # Share of product p used in each machine m across final demand. Sum by product yields 1.
      "{C_Y_agg}" := matsbyname::matrixproduct_byname(matsbyname::hatinv_byname(f_Y),
                                                      Alloc_mat_Y),
      # Share of product p used in each machine m across final demand and EIOU. Sum by product yields 1.
      "{C_EIOU_Y_agg}" := matsbyname::matrixproduct_byname(matsbyname::hatinv_byname(f_EIOU_Y, keep = "rownames"),
                                                           Alloc_mat_EIOU_Y),
    ) |> 
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[C_EIOU_agg]], .data[[C_Y_agg]], .data[[C_EIOU_Y_agg]])
  
  return(C_mats_agg)
}



#' Calculates the aggregated FU efficiency of products when used in EIOU, Y, or economy-wide
#'
#' @param C_mats_agg A data frame containing the aggregated C matrices.
#' @param eta_fu_vecs The data frame containing the efficiency vectors.
#' @param non_energy_use_machine The character string of the non-energy use machine that needs to be excluded
#'                               for efficiencies excluding non-energy uses.
#' @param neu The name of the column containing the information of whether non-energy uses are included or not in the efficiency calculations.
#' @param included A character string stating that non-energy uses are included.
#' @param excluded A character string stating that non-energy uses are excluded.
#' @param eta.fu The name of the column containing the machine efficiencies in the eta_fu_vecs data frame.
#' @param C_EIOU_agg The name of the column containing the C_EIOU aggregated matrix in the output data frame.
#' @param C_Y_agg The name of the column containing the C_Y aggregated matrix in the output data frame.
#' @param C_EIOU_Y_agg The name of the column containing the C_EIOU_Y aggregated matrix in the output data frame.
#' @param C_EIOU_agg_excl_NEU The name of a temporary column containing the C_EIOU aggregated matrix excluding non-energy uses.
#' @param C_Y_agg_excl_NEU The name of a temporary column containing the C_Y aggregated matrix excluding non-energy uses.
#' @param C_EIOU_Y_agg_excl_NEU The name of a temporary column containing the C_EIOU_Y aggregated matrix excluding non-energy uses.
#' @param eta_p_eiou The name of the column containing the efficiency of each product when used in EIOU.
#' @param eta_p_y The name of the column containing the efficiency of each product when used in final demand.
#' @param eta_p_eiou_y The name of the column containing the efficiency of each product when used in either EIOU or final demand.
#' @param country,method,energy_type,last_stage,year See `IEATools::iea_cols`.
#'
#' @return A data frame containing the product efficiencies when used in EIOU, Y, or economy-wide (Y and EIOU).
#' @export
calc_fu_Y_EIOU_agg_efficiencies <- function(C_mats_agg,
                                            eta_fu_vecs,
                                            non_energy_use_machine = "Non-energy consumption -> NEU",
                                            neu = "NEU",
                                            included = "Included",
                                            excluded = "Excluded",
                                            eta.fu = "eta.fu",
                                            C_EIOU_agg = "C_EIOU_agg",
                                            C_Y_agg = "C_Y_agg",
                                            C_EIOU_Y_agg = "C_EIOU_Y_agg",
                                            C_EIOU_agg_excl_NEU = "C_EIOU_agg_excl_NEU",
                                            C_Y_agg_excl_NEU = "C_Y_agg_excl_NEU",
                                            C_EIOU_Y_agg_excl_NEU = "C_EIOU_Y_agg_excl_NEU",
                                            eta_p_eiou = "eta_p_eiou",
                                            eta_p_y = "eta_p_y",
                                            eta_p_eiou_y = "eta_p_eiou_y",
                                            country = IEATools::iea_cols$country,
                                            method = IEATools::iea_cols$method,
                                            energy_type = IEATools::iea_cols$energy_type,
                                            last_stage = IEATools::iea_cols$last_stage,
                                            year = IEATools::iea_cols$year){
  
  # (1) Calculation of efficiencies including non-energy uses
  eta_fu_agg_incl_NEU <- C_mats_agg |> 
    dplyr::left_join(eta_fu_vecs, by = c({country}, {method}, {energy_type}, {last_stage}, {year})) |> 
    dplyr::mutate(
      "{eta_p_eiou}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_agg]], .data[[eta.fu]]) |> 
        matsbyname::clean_byname(margin = 1),
      "{eta_p_y}" := matsbyname::matrixproduct_byname(.data[[C_Y_agg]], .data[[eta.fu]]) |> 
        matsbyname::clean_byname(margin = 1),
      "{eta_p_eiou_y}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_Y_agg]], .data[[eta.fu]]) |> 
        matsbyname::clean_byname(margin = 1),
      "{neu}" := included
    )
  
  # (2) Calculation of efficiencies excluding non-energy uses
  # (2.a) Determination of aggregated C_mats excluding non-energy uses
  C_mats_agg_excl_NEU <- C_mats_agg |> 
    dplyr::mutate(
      # New C_Y_agg excluding non-energy uses
      "{C_Y_agg_excl_NEU}" := matsbyname::matrixproduct_byname(
        matsbyname::select_cols_byname(.data[[C_Y_agg]], remove_pattern = list(non_energy_use_machine)) |> 
          matsbyname::rowsums_byname() |> 
          matsbyname::hatinv_byname(), 
        matsbyname::select_cols_byname(.data[[C_Y_agg]], remove_pattern = list(non_energy_use_machine))),
      # New C_EIOU_agg excluding non-energy uses
      "{C_EIOU_agg_excl_NEU}" := matsbyname::matrixproduct_byname(
        matsbyname::select_cols_byname(.data[[C_EIOU_agg]], remove_pattern = list(non_energy_use_machine)) |>
          matsbyname::rowsums_byname() |>
          matsbyname::hatinv_byname(keep = "rownames"),
        matsbyname::select_cols_byname(.data[[C_EIOU_agg]], remove_pattern = list(non_energy_use_machine))),
      # New C_EIOU_Y_agg excluding non-energy uses
      "{C_EIOU_Y_agg_excl_NEU}" := matsbyname::matrixproduct_byname(
        matsbyname::select_cols_byname(.data[[C_EIOU_Y_agg]], remove_pattern = list(non_energy_use_machine)) |> 
          matsbyname::rowsums_byname() |> 
          matsbyname::hatinv_byname(), 
        matsbyname::select_cols_byname(.data[[C_EIOU_Y_agg]], remove_pattern = list(non_energy_use_machine)))
    ) |> 
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[C_EIOU_agg_excl_NEU]],
                  .data[[C_Y_agg_excl_NEU]], .data[[C_EIOU_Y_agg_excl_NEU]]) |> 
    dplyr::rename(
      "{C_EIOU_agg}" := .data[[C_EIOU_agg_excl_NEU]],
      "{C_Y_agg}" := .data[[C_Y_agg_excl_NEU]],
      "{C_EIOU_Y_agg}" := .data[[C_EIOU_Y_agg_excl_NEU]]
    )
  
  # (2.b) Determination of aggregated efficiencies
  eta_fu_agg_excl_NEU <- C_mats_agg_excl_NEU |> 
    dplyr::left_join(eta_fu_vecs, by = c({country}, {method}, {energy_type}, {last_stage}, {year})) |> 
    dplyr::mutate(
      "{eta_p_eiou}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_agg]], .data[[eta.fu]]) |> 
        matsbyname::clean_byname(margin = 1),
      "{eta_p_y}" := matsbyname::matrixproduct_byname(.data[[C_Y_agg]], .data[[eta.fu]]) |> 
        matsbyname::clean_byname(margin = 1),
      "{eta_p_eiou_y}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_Y_agg]], .data[[eta.fu]]) |> 
        matsbyname::clean_byname(margin = 1),
      "{neu}" := excluded
    )
  
  # (3) Binding both data frames
  eta_fu_agg <- dplyr::bind_rows(
    eta_fu_agg_incl_NEU,
    eta_fu_agg_excl_NEU
  )

  return(eta_fu_agg)
}

