
# Code to uncomment:
#   
## calc_fu_Y_EIOU_agg_efficiencies function
# Lines "{C_EIOU_agg_excl_NEU}" :=
# Lines "{eta_p_eiou}" :=
# Lines "{eta_p_eiou}" :=  
# In dplyr::rename(), uncomment: "{C_EIOU_agg}" := .data[[C_EIOU_agg_excl_NEU]],
# 
## calc_eta_fu_eff_phi_Y_EIOU_agg function
# "{C_EIOU_agg_excl_NEU}" :=
# "{eta_phi_p_eiou}" :=
# In dplyr::rename(), uncomment: "{C_EIOU_agg}" := .data[[C_EIOU_agg_excl_NEU]],

## calc_Ef_to_Eu_exiobase
# See comment: "Expanding the EIOU-wide efficiencies data frame to prepare the join"
# 

#' Reads the list of Exiobase energy flows
#'
#' @param path_to_list_exiobase_energy_flows Contains the path to the folder containing the list of Exiobase energy flows
#'
#' @return A data frame containing the list of Exiobase energy flows, and a boolean column stating whether the flow is a final energy flow or not
#' @export
read_list_exiobase_energy_flows <- function(path_to_list_exiobase_energy_flows){
  
  list_exiobase_energy_flows <- readxl::read_excel(path_to_list_exiobase_energy_flows)
  
  return(list_exiobase_energy_flows)
}


#' Calculates the average FU energy efficiency times phi value
#'
#' @param C_mats_agg A data frame containing the aggregated C matrices.
#' @param eta_fu_vecs A data frame containing the efficiency vectors.
#' @param phi_vecs A data frame containing the phi vectors
#' @param non_energy_use_machine The character string of the non-energy use machine that needs to be excluded
#'                               for efficiencies excluding non-energy uses.
#' @param eta.fu The name of the column containing the machine efficiencies in the eta_fu_vecs data frame.
#' @param C_EIOU_agg The name of the column containing the C_EIOU aggregated matrix in the output data frame.
#' @param C_Y_agg The name of the column containing the C_Y aggregated matrix in the output data frame.
#' @param C_EIOU_Y_agg The name of the column containing the C_EIOU_Y aggregated matrix in the output data frame.
#' @param C_EIOU_agg_excl_NEU The name of a temporary column containing the C_EIOU aggregated matrix excluding non-energy uses.
#' @param C_Y_agg_excl_NEU The name of a temporary column containing the C_Y aggregated matrix excluding non-energy uses.
#' @param C_EIOU_Y_agg_excl_NEU The name of a temporary column containing the C_EIOU_Y aggregated matrix excluding non-energy uses.
#' @param eta_phi_p_eiou The name of the column containing the efficiency of each product when used in EIOU.
#' @param eta_phi_p_y The name of the column containing the efficiency of each product when used in final demand.
#' @param eta_phi_p_eiou_y The name of the column containing the efficiency of each product when used in either EIOU or final demand.
#' @param phi The name of the column containing the phi values.
#' @param country,method,energy_type,last_stage,year,product See `IEATools::iea_cols`.
#'
#' @return A data frame containing the average FU efficiencies times phi values for each product when used in EIOU, Y, or economy-wide (Y and EIOU).
#' @export
calc_eta_fu_eff_phi_Y_EIOU_agg <- function(C_mats_agg,
                                           eta_fu_vecs,
                                           phi_vecs,
                                           non_energy_use_machine = "Non-energy consumption -> NEU",
                                           eta.fu = "eta.fu",
                                           C_EIOU_agg = "C_EIOU_agg",
                                           C_Y_agg = "C_Y_agg",
                                           C_EIOU_Y_agg = "C_EIOU_Y_agg",
                                           C_EIOU_agg_excl_NEU = "C_EIOU_agg_excl_NEU",
                                           C_Y_agg_excl_NEU = "C_Y_agg_excl_NEU",
                                           C_EIOU_Y_agg_excl_NEU = "C_EIOU_Y_agg_excl_NEU",
                                           eta_phi_p_eiou = "eta_phi_p_eiou",
                                           eta_phi_p_y = "eta_phi_p_y",
                                           eta_phi_p_eiou_y = "eta_phi_p_eiou_y",
                                           phi = "phi",
                                           country = IEATools::iea_cols$country,
                                           method = IEATools::iea_cols$method,
                                           energy_type = IEATools::iea_cols$energy_type,
                                           last_stage = IEATools::iea_cols$last_stage,
                                           year = IEATools::iea_cols$year,
                                           product = IEATools::iea_cols$product){
  
  # (1) Determination of aggregated C_mats excluding non-energy uses
  C_mats_agg_excl_NEU <- C_mats_agg |> 
    dplyr::mutate(
      # New C_Y_agg excluding non-energy uses
      "{C_Y_agg_excl_NEU}" := matsbyname::matrixproduct_byname(
        matsbyname::select_cols_byname(.data[[C_Y_agg]], remove_pattern = list(non_energy_use_machine)) |> 
          matsbyname::rowsums_byname() |> 
          matsbyname::hatinv_byname(), 
        matsbyname::select_cols_byname(.data[[C_Y_agg]], remove_pattern = list(non_energy_use_machine))),
      # New C_EIOU_agg excluding non-energy uses
      # "{C_EIOU_agg_excl_NEU}" := matsbyname::matrixproduct_byname(
      #   matsbyname::select_cols_byname(.data[[C_EIOU_agg]], remove_pattern = list(non_energy_use_machine)) |>
      #     matsbyname::rowsums_byname() |>
      #     matsbyname::hatinv_byname(keep = "rownames"),
        # matsbyname::select_cols_byname(.data[[C_EIOU_agg]], remove_pattern = list(non_energy_use_machine))),
      # New C_EIOU_Y_agg excluding non-energy uses
      "{C_EIOU_Y_agg_excl_NEU}" := matsbyname::matrixproduct_byname(
        matsbyname::select_cols_byname(.data[[C_EIOU_Y_agg]], remove_pattern = list(non_energy_use_machine)) |> 
          matsbyname::rowsums_byname() |> 
          matsbyname::hatinv_byname(), 
        matsbyname::select_cols_byname(.data[[C_EIOU_Y_agg]], remove_pattern = list(non_energy_use_machine)))
    ) |> 
    dplyr::select(tidyselect::any_of(c(country, method, energy_type, last_stage, year, C_EIOU_agg_excl_NEU, C_Y_agg_excl_NEU, C_EIOU_Y_agg_excl_NEU))) |> 
    # dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[C_EIOU_agg_excl_NEU]],
    #               .data[[C_Y_agg_excl_NEU]], .data[[C_EIOU_Y_agg_excl_NEU]]) |> 
    dplyr::rename(
      #"{C_EIOU_agg}" := .data[[C_EIOU_agg_excl_NEU]],
      "{C_Y_agg}" := .data[[C_Y_agg_excl_NEU]],
      "{C_EIOU_Y_agg}" := .data[[C_EIOU_Y_agg_excl_NEU]]
    )
  
  
  # (2) Calculations of eta * phi
  # Represents the final-to-useful energy efficiency times the useful stage Phi coefficient
  eta_fu_eff_phi_Y_EIOU_agg <- C_mats_agg_excl_NEU |> 
    dplyr::left_join(eta_fu_vecs, by = c({country}, {method}, {energy_type}, {last_stage}, {year})) |> 
    dplyr::left_join(phi_vecs, by = c({country}, {year})) |> 
    dplyr::mutate(
      # "{eta_phi_p_eiou}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_agg]], matsbyname::hatize_byname(.data[[eta.fu]])) |> 
      #   matsbyname::clean_byname(margin = 1) |> 
      #   matsbyname::rename_to_piece_byname(piece = "suff", margin = 2, notation = list(RCLabels::arrow_notation)) |> 
      #   matsbyname::aggregate_byname(margin = 2) |> 
      #   matsbyname::setcoltype(product) |> 
      #   matsbyname::matrixproduct_byname(.data[[phi]]),
      "{eta_phi_p_y}" := matsbyname::matrixproduct_byname(.data[[C_Y_agg]], matsbyname::hatize_byname(.data[[eta.fu]], keep = "rownames")) |> 
        matsbyname::clean_byname(margin = 1) |> 
        matsbyname::rename_to_piece_byname(piece = "suff", margin = 2, notation = list(RCLabels::arrow_notation)) |> 
        matsbyname::aggregate_byname(margin = 2) |> 
        matsbyname::setcoltype(product) |> 
        matsbyname::matrixproduct_byname(.data[[phi]]),
      "{eta_phi_p_eiou_y}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_Y_agg]], matsbyname::hatize_byname(.data[[eta.fu]], keep = "rownames")) |> 
        matsbyname::clean_byname(margin = 1) |> 
        matsbyname::rename_to_piece_byname(piece = "suff", margin = 2, notation = list(RCLabels::arrow_notation)) |> 
        matsbyname::aggregate_byname(margin = 2) |> 
        matsbyname::setcoltype(product) |> 
        matsbyname::matrixproduct_byname(.data[[phi]]),
    ) |> 
    dplyr::select(tidyselect::any_of(c(country, method, year, eta_phi_p_eiou, eta_phi_p_y, eta_phi_p_eiou_y)))
    #dplyr::select(.data[[country]], .data[[method]], .data[[year]], .data[[eta_phi_p_eiou]], .data[[eta_phi_p_y]], .data[[eta_phi_p_eiou_y]])
  
  # (3) Determination of aggregated efficiencies (excluding non-energy uses)
  # eta_fu_eff_phi_Y_EIOU_agg <- C_mats_agg_excl_NEU |> 
  #   dplyr::left_join(eta_fu_vecs, by = c({country}, {method}, {energy_type}, {last_stage}, {year})) |> 
  #   dplyr::mutate(
  #     "{eta_p_eiou}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_agg]], .data[[eta.fu]]) |> 
  #       matsbyname::clean_byname(margin = 1),
  #     "{eta_p_y}" := matsbyname::matrixproduct_byname(.data[[C_Y_agg]], .data[[eta.fu]]) |> 
  #       matsbyname::clean_byname(margin = 1),
  #     "{eta_p_eiou_y}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_Y_agg]], .data[[eta.fu]]) |> 
  #       matsbyname::clean_byname(margin = 1)
  #   )
  
  return(eta_fu_eff_phi_Y_EIOU_agg)
}



#' Calculates the final energy to final exergy multipliers
#'
#' @param phi_vecs A data frame of phi (exergy-to-energy ratio) coefficients.
#' @param years_exiobase The years for which the coefficients are provided to the Exiobase team
#' @param full_list_exiobase_flows The full list of energy flows used in the Exiobase pipeline led by KR
#' @param country_concordance_table_df A data frame containing the country concordance table
#' @param final_energy_flow The name of the column stating whether a flow is a final energy flow or not
#' @param exiobase_flow The name of the column stating the name of the Exiobase flow
#' @param pfu_code The name of the column containing the PFU country name
#' @param iea_country_name The name of the column containing the IEA country name
#' @param iea_country_name_accented The name of the column containing the IEA country name with accents
#' @param phi The name of the column containing the phi values
#' @param matnames The name of the column containing matrices names after unpacking the matrices
#' @param matvals The name of the column containing matrices values after unpacking the matrices
#' @param colnames The name of the column containing the column names after unpacking the matrices
#' @param rowtypes The name of the column containing the matrices row types names after unpacking the matrices
#' @param coltypes The name of the column containing matrices column types after unpacking the matrices
#' @param country,year,product,flow See `IEATools::iea_cols`.
#'
#' @return A data frame of final energy to final exergy multipliers
#'
#' @export
calc_Ef_to_Xf_exiobase <- function(phi_vecs,
                                   years_exiobase,
                                   full_list_exiobase_flows,
                                   country_concordance_table_df,
                                   final_energy_flow = "Final.energy.flow",
                                   exiobase_flow = "Exiobase.Flow",
                                   pfu_code = "PFU.code",
                                   iea_country_name = "IEA.country.name",
                                   iea_country_name_accented = "IEA.name.accented",
                                   phi = "phi",
                                   matnames = "matnames",
                                   colnames = "colnames",
                                   matvals = "matvals",
                                   rowtypes = "rowtypes",
                                   coltypes = "coltypes",
                                   country = IEATools::iea_cols$country,
                                   product = IEATools::iea_cols$product,
                                   year = IEATools::iea_cols$year,
                                   flow = IEATools::iea_cols$flow){
  
  # Filtering out non final energy flows
  # We keep losses and non-energy uses in the multipliers we produce for Exiobase
  list_final_energy_flows <- full_list_exiobase_flows %>%
    dplyr::filter(.data[[final_energy_flow]] == TRUE) %>%
    dplyr::select(tidyselect::all_of(exiobase_flow))
  
  # Expanding to determine phi values for each Exiobase flow
  phi_vals_df <- phi_vecs %>%
    dplyr::filter(.data[[year]] %in% years_exiobase) %>%
    tidyr::pivot_longer(cols = phi, names_to = matnames, values_to = matvals) %>%
    matsindf::expand_to_tidy(rownames = product) %>%
    dplyr::select(-tidyselect::any_of(c(matnames, colnames, rowtypes, coltypes))) %>%
    tidyr::expand_grid(list_final_energy_flows) %>%
    dplyr::filter(.data[[product]] %in% IEATools::products) %>%
    dplyr::rename("{pfu_code}" := country) %>%
    dplyr::left_join(country_concordance_table_df %>% dplyr::select(.data[[iea_country_name_accented]], .data[[pfu_code]]),
                     by = {pfu_code}) %>%
    dplyr::select(-.data[[pfu_code]]) %>%
    dplyr::rename("{iea_country_name}" := .data[[iea_country_name_accented]],
                  "{flow}" := exiobase_flow) %>%
    dplyr::filter(!is.na(.data[[iea_country_name]])) %>%
    dplyr::relocate(.data[[iea_country_name]], .before = .data[[year]]) %>%
    tidyr::pivot_wider(names_from = .data[[year]], values_from = .data[[matvals]])
  
  return(phi_vals_df)
}


#' Calculates the final energy to useful energy multipliers
#'
#' @param eta_fu_Y_EIOU_mats The input data frame containing matrices with all the efficiencies by final demand sector and energy industry
#' @param eta_fu_Y_EIOU_agg The input data frame containing matrices with the economy-wide efficiencies by energy product
#' @param years_exiobase The years for which the coefficients are provided to the Exiobase team
#' @param full_list_exiobase_flows The full list of energy flows used in the Exiobase pipeline led by KR
#' @param country_concordance_table_df A data frame containing the country concordance table
#' @param useful_energy_flow The name of the column stating whether a flow is a final energy flow or not
#' @param exiobase_flow The name of the column stating the name of the Exiobase flow
#' @param pfu_code The name of the column containing the PFU country name
#' @param pfu_flow The name of the column containing the PFU flow names
#' @param iea_country_name The name of the column containing the IEA country name
#' @param iea_country_name_accented The name of the column containing the IEA country name with accents
#' @param phi The name of the column containing the phi values
#' @param matnames The name of the column containing matrices names after unpacking the matrices
#' @param colnames The name of the column containing the column names after unpacking the matrices
#' @param matvals The name of the column containing matrices values after unpacking the matrices
#' @param rowtypes The name of the column containing the matrices row types names after unpacking the matrices
#' @param coltypes The name of the column containing matrices column types after unpacking the matrices
#' @param eta_p_eiou The name of the column containing the efficiencies for products used in EIOU
#' @param eta_p_eiou_y The name of the column containing the efficiencies for products used in final demand.
#' @param country,year,product,flow,method,energy_type,last_stage See `IEATools::iea_cols`.
#' @param energy_type_E The letter standing for energy as energy type.
#' @param eta_fu_Y_E The name of the the column containing the efficiencies of products when used as part of final demand.
#' @param eta_fu_EIOU_E The name of the column containing the efficiencies of products when used as part of the EIOU matrix.
#' @param eta The name of the column containing the efficiencies.
#'
#' @return A dataframe of the final energy to useful energy multipliers
#' @export
calc_Ef_to_Eu_exiobase <- function(eta_fu_Y_EIOU_mats,
                                   eta_fu_Y_EIOU_agg,
                                   years_exiobase,
                                   full_list_exiobase_flows,
                                   country_concordance_table_df,
                                   useful_energy_flow = "Useful.energy.flow",
                                   exiobase_flow = "Exiobase.Flow",
                                   pfu_code = "PFU.code",
                                   pfu_flow = "PFU.flow",
                                   iea_country_name = "IEA.country.name",
                                   iea_country_name_accented = "IEA.name.accented",
                                   phi = "phi",
                                   matnames = "matnames",
                                   colnames = "colnames",
                                   matvals = "matvals",
                                   rowtypes = "rowtypes",
                                   coltypes = "coltypes",
                                   energy_type_E = IEATools::energy_types$e,
                                   country = IEATools::iea_cols$country,
                                   product = IEATools::iea_cols$product,
                                   year = IEATools::iea_cols$year,
                                   flow = IEATools::iea_cols$flow,
                                   method = IEATools::iea_cols$method,
                                   energy_type = IEATools::iea_cols$energy_type,
                                   last_stage = IEATools::iea_cols$last_stage,
                                   eta_p_eiou = "eta_p_eiou",
                                   eta_p_eiou_y = "eta_p_eiou_y",
                                   eta_fu_Y_E = "eta_fu_Y_E",
                                   eta_fu_EIOU_E = "eta_fu_EIOU_E",
                                   eta = "eta"){
  
  # Filtering out non useful energy flows
  # So here we remove non-energy uses and losses
  list_useful_energy_flows <- full_list_exiobase_flows %>%
    dplyr::filter(.data[[useful_energy_flow]] == TRUE) %>%
    dplyr::select(tidyselect::all_of(c(exiobase_flow, pfu_flow)))
  
  # Expanding the EIOU-wide efficiencies data frame to prepare the join
  eta_fu_EIOU_wide_df <- eta_fu_Y_EIOU_agg |> 
    dplyr::filter(.data[[year]] %in% years_exiobase) |> 
    dplyr::filter(.data[[energy_type]] == energy_type_E) |> 
    #REPLACE THE TWO LINES COMMENTED OUT FOR FINAL VERSION.
    #dplyr::select(tidyselect::any_of(c(country, method, energy_type, year, eta_p_eiou))) |> 
    dplyr::select(tidyselect::any_of(c(country, method, energy_type, year, eta_p_eiou_y))) |> 
    #tidyr::pivot_longer(cols = tidyselect::any_of(eta_p_eiou), values_to = matvals, names_to = matnames) |> 
    tidyr::pivot_longer(cols = tidyselect::any_of(eta_p_eiou_y), values_to = matvals, names_to = matnames) |> 
    # This will need to go, too.
    dplyr::filter(! is.null(.data[[matvals]])) |> 
    matsindf::expand_to_tidy() |> 
    dplyr::rename(
      "{product}" := rownames,
      "{eta}" := matvals
    ) |> 
    dplyr::select(tidyselect::all_of(c(country, method, energy_type, year, product, eta))) |>
    dplyr::mutate("{pfu_flow}" := "EIOU-wide",
                  "{last_stage}" := "Final")
  
  # Expanding the final-to-useful efficiencies to prepare the join
  eta_fu_df <- eta_fu_Y_EIOU_mats |>
    dplyr::filter(.data[[year]] %in% years_exiobase) |> 
    dplyr::select(tidyselect::any_of(c(country, method, energy_type, last_stage, year, eta_fu_Y_E, eta_fu_EIOU_E))) |> 
    tidyr::pivot_longer(cols = tidyr::ends_with("_E"), names_to = matnames, values_to = matvals) |> 
    # This will need to go
    dplyr::filter(! is.null(.data[[matvals]])) |> 
    matsindf::expand_to_tidy(rownames = product, colnames = pfu_flow) |> 
    dplyr::select(-tidyselect::all_of(c(rowtypes, coltypes, matnames))) |> 
    dplyr::rename("{eta}" := matvals) |> 
    dplyr::filter(.data[[product]] %in% IEATools::products) |>
    dplyr::bind_rows(eta_fu_EIOU_wide_df) |> 
    dplyr::filter(.data[[eta]] != 0)
  
  # Expanding the economy-wide efficiencies data frame to prepare the join
  eta_fu_economy_wide_df <- eta_fu_Y_EIOU_agg |> 
    dplyr::filter(.data[[year]] %in% years_exiobase) |> 
    dplyr::filter(.data[[energy_type]] == energy_type_E) |> 
    dplyr::select(tidyselect::any_of(c(country, method, energy_type, year, eta_p_eiou_y))) |> 
    tidyr::pivot_longer(cols = eta_p_eiou_y, values_to = matvals, names_to = matnames) |> 
    # This will need to go
    dplyr::filter(! is.null(.data[[matvals]])) |> 
    matsindf::expand_to_tidy() |> 
    dplyr::rename(
      "{product}" := rownames,
      "{pfu_code}" := country,
      "{eta}" := matvals
    ) |> 
    dplyr::select(tidyselect::all_of(c(pfu_code, method, energy_type, year, product, eta))) |> 
    dplyr::mutate("{exiobase_flow}" := "Economy-wide") |> 
    dplyr::filter(.data[[eta]] != 0)
  
  # Preparing the (Country, Year, Product) list
  country_year_product_list <- eta_fu_df |> 
    dplyr::filter(.data[[year]] %in% years_exiobase) |> 
    dplyr::select(tidyselect::all_of(c(country, year, product))) |> 
    dplyr::distinct()
  
  # List of Exiobase flows to be joined to the PFU results
  expanded_exiobase_Ue_flows <- list_useful_energy_flows |> 
    tidyr::expand_grid(country_year_product_list) |> 
    dplyr::relocate(tidyselect::all_of(c(exiobase_flow, pfu_flow)), .after = .data[[year]])
  
  # Now, joining to ascribe each Exiobase flow a PFU flow
  Ef_to_Eu_multipliers <- expanded_exiobase_Ue_flows |> 
    dplyr::left_join(eta_fu_df, by = c({country}, {year}, {product}, {pfu_flow})) |> 
    dplyr::filter(!is.na(.data[[eta]])) |> 
    dplyr::rename("{pfu_code}" := .data[[country]]) |> 
    dplyr::select(-tidyselect::all_of(pfu_flow)) |> 
    dplyr::bind_rows(eta_fu_economy_wide_df) |> 
    dplyr::left_join(country_concordance_table_df %>% dplyr::select(tidyselect::all_of(c(iea_country_name_accented, pfu_code))),
                     by = pfu_code) |>
    dplyr::select(-tidyselect::all_of(pfu_code)) |>
    dplyr::rename("{iea_country_name}" := .data[[iea_country_name_accented]],
                  "{flow}" := .data[[exiobase_flow]]) |>
    dplyr::filter(!is.na(.data[[iea_country_name]])) |>
    dplyr::select(tidyselect::all_of(c(iea_country_name, year, product, flow, eta))) |>
    dplyr::relocate(tidyselect::all_of(flow), .before = .data[[product]]) |>
    tidyr::pivot_wider(names_from = .data[[year]], values_from = .data[[eta]])
  
  return(Ef_to_Eu_multipliers)
}


#' Calculates the final energy to energy losses multipliers
#'
#' @param ExiobaseEftoEuMultipliers_df The data frame of final energy to useful energy multipliers previously calculated
#'
#' @return A data frame of the final energy to energy losses multipliers
#' @export
calc_Ef_to_Eloss_exiobase <- function(ExiobaseEftoEuMultipliers_df){
  
  Ef_to_Eloss_multipliers <- ExiobaseEftoEuMultipliers_df |> 
    # Conditional if mutate to avoid putting 100% where efficiency is 0; which corresponds to a missing efficiency anyway.
    # However we are now filtering out 0 efficiencies in the calc_Ef_to_Eu_exiobase() function to help identify potentially missing efficiencies
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.double), ~ dplyr::case_when(.x == 0 ~ .x, 
                                             .x >= 1 ~ 0,
                                             TRUE ~ 1 - .x)
      )
    )
  return(Ef_to_Eloss_multipliers)
}


#' Calculates the final energy to useful exergy multipliers
#'
#' @param EtafuYEIOU_mats The input data frame containing matrices with all the efficiencies by final demand sector and energy industry
#' @param phi_vecs A data frame of phi (exergy-to-energy ratio) coefficients.
#' @param eta_fu_phi_Y_EIOU_agg A data frame containing matrices with the economy-wide efficiencies time phi values by energy product
#' @param years_exiobase The years for which the coefficients are provided to the Exiobase team
#' @param full_list_exiobase_flows The full list of energy flows used in the Exiobase pipeline led by KR
#' @param country_concordance_table_df A data frame containing the country concordance table
#' @param useful_energy_flow The name of the column stating whether a flow is a final energy flow or not
#' @param exiobase_flow The name of the column stating the name of the Exiobase flow
#' @param pfu_code The name of the column containing the PFU country name
#' @param pfu_flow The name of the column containing the PFU flow names
#' @param iea_country_name The name of the column containing the IEA country name
#' @param iea_country_name_accented The name of the column containing the IEA country name with accents
#' @param phi The name of the column containing the phi values
#' @param matnames The name of the column containing matrices names after unpacking the matrices
#' @param matvals The name of the column containing matrices values after unpacking the matrices
#' @param colnames The name of the column containing the column names after unpacking the matrices
#' @param rowtypes The name of the column containing the matrices row types names after unpacking the matrices
#' @param coltypes The name of the column containing matrices column types after unpacking the matrices
#' @param eta_p_eiou The name of the column containing the efficiencies.
#' @param country,year,product,flow,method,energy_type,last_stage See `IEATools::iea_cols`.
#' @param energy_type_E The letter standing for energy as energy type.
#' @param eta_fu_Y_E The name of the the column containing the efficiencies of products when used as part of final demand.
#' @param eta_fu_EIOU_E The name of the column containing the efficiencies of products when used as part of the EIOU matrix.
#' @param eta_p_eiou_y The name of the column containing the efficiencies of products when used as part of the final demand
#' @param eta_phi_p_eiou The name of the column with the matrices containing the efficiencies, multiplied by the phi values, of products when used as part of the EIOU matrix.
#' @param eta_phi_p_eiou_y The name of the column with the matrices containing the efficiencies, multiplied by the phi values, of products when used as part of the Y matrix.
#' @param eta_phi_p_eiou The name of the column containing the efficiency values multiplied by the phi values for products used in EIOU.
#' @param eta_phi_p_eiou_y The name of the column containing the efficiency values multiplied by the phi values for products used economy-wide.
#' @param eta_fu_Y_X The name of the column containing the exergy efficiencies for products used as part of final demand.
#' @param eta_fu_EIOU The name of the column containing the exergy efficiencies for products used as part of EIOU.
#' @param phi_eta_X The name of the column containing the efficiencies, multiplied by the phi values, of products when used as part of the EIOU matrix.
#' @param eta The name of the column containing the efficiencies.
#'
#' @return A data frame of the final energy to useful exergy multipliers
#' @export
calc_Ef_to_Xu_exiobase <- function(EtafuYEIOU_mats,
                                   phi_vecs,
                                   eta_fu_phi_Y_EIOU_agg,
                                   years_exiobase,
                                   full_list_exiobase_flows,
                                   country_concordance_table_df,
                                   useful_energy_flow = "Useful.energy.flow",
                                   exiobase_flow = "Exiobase.Flow",
                                   pfu_code = "PFU.code",
                                   pfu_flow = "PFU.flow",
                                   iea_country_name = "IEA.country.name",
                                   iea_country_name_accented = "IEA.name.accented",
                                   phi = "phi",
                                   matnames = "matnames",
                                   colnames = "colnames",
                                   matvals = "matvals",
                                   rowtypes = "rowtypes",
                                   coltypes = "coltypes",
                                   energy_type_E = IEATools::energy_types$e,
                                   country = IEATools::iea_cols$country,
                                   product = IEATools::iea_cols$product,
                                   year = IEATools::iea_cols$year,
                                   flow = IEATools::iea_cols$flow,
                                   method = IEATools::iea_cols$method,
                                   energy_type = IEATools::iea_cols$energy_type,
                                   last_stage = IEATools::iea_cols$last_stage,
                                   eta_p_eiou = "eta_p_eiou",
                                   eta_p_eiou_y = "eta_p_eiou_y",
                                   eta_fu_Y_E = "eta_fu_Y_E",
                                   eta_fu_EIOU_E = "eta_fu_EIOU_E",
                                   eta_fu_Y_X = "eta_fu_Y_X",
                                   eta_fu_EIOU_X = "eta_fu_EIOU_X",
                                   eta_phi_p_eiou = "eta_phi_p_eiou",
                                   eta_phi_p_eiou_y = "eta_phi_p_eiou_y",
                                   phi_eta_X = "phi_eta_X",
                                   eta = "eta"){
  
  # Filtering out non useful energy flows
  # So here we remove non-energy uses and losses
  list_useful_energy_flows <- full_list_exiobase_flows %>%
    dplyr::filter(.data[[useful_energy_flow]] == TRUE) %>%
    dplyr::select(tidyselect::all_of(c(exiobase_flow, pfu_flow)))
  
  # Expanding the EIOU-wide efficiencies*phi values data frame to prepare the join
  eta_times_phi_EIOU_wide_df <- eta_fu_phi_Y_EIOU_agg |> 
    dplyr::filter(.data[[year]] %in% years_exiobase) |> 
    # Thiw will need to be replaced
    #dplyr::select(tidyselect::any_of(c(country, method, year, eta_phi_p_eiou))) |> 
    dplyr::select(tidyselect::any_of(c(country, method, year, eta_phi_p_eiou_y))) |> 
    #tidyr::pivot_longer(cols = tidyselect::any_of(eta_phi_p_eiou), values_to = matvals, names_to = matnames) |> 
    tidyr::pivot_longer(cols = tidyselect::any_of(eta_phi_p_eiou_y), values_to = matvals, names_to = matnames) |> 
    # This will need to go
    dplyr::filter(! is.null(.data[[matvals]])) |> 
    matsindf::expand_to_tidy() |> 
    dplyr::rename(
      "{product}" := rownames,
      "{phi_eta_X}" := matvals
    ) |> 
    dplyr::select(tidyselect::all_of(c(country, method, year, product, phi_eta_X))) |>
    dplyr::mutate("{pfu_flow}" := "EIOU-wide",
                  "{last_stage}" := "Final",
                  "{energy_type}" := "E")
  
  # Expanding Phivecs
  phi_vals_df <- phi_vecs |> 
    dplyr::filter(.data[[year]] %in% years_exiobase) |> 
    tidyr::pivot_longer(cols = tidyselect::any_of(phi), names_to = matnames, values_to = matvals) |> 
    matsindf::expand_to_tidy(rownames = product) |> 
    dplyr::select(-tidyselect::all_of(c(matnames, colnames, rowtypes, coltypes))) |> 
    dplyr::rename("{pfu_code}" := country,
                  "{phi}" := matvals)
  
  # Expanding the final-to-useful efficiencies to prepare the join
  phi_eta_fu_df <- EtafuYEIOU_mats |>
    dplyr::filter(.data[[year]] %in% years_exiobase) |> 
    dplyr::select(tidyselect::any_of(c(country, method, energy_type, last_stage, year, eta_fu_Y_X, eta_fu_EIOU_X))) |> 
    tidyr::pivot_longer(cols = tidyr::ends_with("_X"), names_to = matnames, values_to = matvals) |> 
    # This will need to go
    dplyr::filter(! is.null(.data[[matvals]])) |> 
    matsindf::expand_to_tidy(rownames = product, colnames = pfu_flow) |> 
    dplyr::select(-tidyselect::all_of(c(rowtypes, coltypes, matnames))) |> 
    dplyr::rename("{eta}" := matvals) |> 
    dplyr::filter(.data[[product]] %in% IEATools::products) |>
    dplyr::left_join(phi_vals_df |> dplyr::rename("{country}" := pfu_code), by = c({country}, {year}, {product})) |> 
    dplyr::mutate(
      "{phi_eta_X}" := .data[[phi]] * .data[[eta]]
    ) |> 
    dplyr::select(-tidyselect::all_of(c(eta, phi))) |> 
    dplyr::bind_rows(eta_times_phi_EIOU_wide_df) |> 
    dplyr::filter(.data[[phi_eta_X]] != 0)
  
  # Expanding the economy-wide efficiencies*phi values data frame to prepare the join
  eta_times_phi_economy_wide_df <- eta_fu_phi_Y_EIOU_agg |> 
    dplyr::filter(.data[[year]] %in% years_exiobase) |> 
    dplyr::select(tidyselect::any_of(c(country, method, year, eta_phi_p_eiou_y))) |> 
    tidyr::pivot_longer(cols = tidyselect::any_of(eta_phi_p_eiou_y), values_to = matvals, names_to = matnames) |> 
    matsindf::expand_to_tidy() |> 
    dplyr::rename(
      "{product}" := rownames,
      "{pfu_code}" := country,
      "{phi_eta_X}" := matvals
    ) |> 
    dplyr::select(tidyselect::all_of(c(pfu_code, method, year, product, phi_eta_X))) |>
    dplyr::mutate("{exiobase_flow}" := "Economy-wide") |> 
    dplyr::filter(.data[[phi_eta_X]] != 0)
  
  # Preparing the (Country, Year, Product) list
  country_year_product_list <- phi_eta_fu_df |> 
    dplyr::filter(.data[[year]] %in% years_exiobase) |> 
    dplyr::select(tidyselect::all_of(c(country, year, product))) |> 
    dplyr::distinct()
  
  # List of Exiobase flows to be joined to the PFU results
  expanded_exiobase_Ue_flows <- list_useful_energy_flows |> 
    tidyr::expand_grid(country_year_product_list) |> 
    dplyr::relocate(tidyselect::all_of(c(exiobase_flow,pfu_flow)), .after = tidyselect::all_of(year))
  
  # Now, joining to ascribe each Exiobase flow a PFU flow
  Ef_to_Xu_multipliers <- expanded_exiobase_Ue_flows |> 
    dplyr::left_join(phi_eta_fu_df, by = c({country}, {year}, {product}, {pfu_flow})) |> 
    dplyr::filter(!is.na(.data[[phi_eta_X]])) |> 
    dplyr::rename("{pfu_code}" := country) |> 
    dplyr::select(-tidyselect::all_of(pfu_flow)) |> 
    dplyr::bind_rows(eta_times_phi_economy_wide_df) |> 
    dplyr::left_join(country_concordance_table_df %>% dplyr::select(tidyselect::all_of(c(iea_country_name_accented, pfu_code))),
                     by = pfu_code) |>
    dplyr::rename("{iea_country_name}" := iea_country_name_accented,
                  "{flow}" := exiobase_flow) |>
    dplyr::filter(!is.na(.data[[iea_country_name]])) |>
    dplyr::select(tidyselect::all_of(c(iea_country_name, year, product, flow, phi_eta_X))) |>
    dplyr::relocate(tidyselect::all_of(flow), .before = tidyselect::all_of(product)) |>
    tidyr::pivot_wider(names_from = .data[[year]], values_from = .data[[phi_eta_X]])
  
  return(Ef_to_Xu_multipliers)
}



#' Calculates the final energy to exergy losses multipliers
#'
#' @param ExiobaseEftoXuMultipliers_df The data frame of final energy to useful exergy multipliers previously calculated
#'
#' @return A data frame of the final energy to exergy losses multipliers
#' @export
calc_Ef_to_Xloss_exiobase <- function(ExiobaseEftoXuMultipliers_df){
  
  Ef_to_Xloss_multipliers <- ExiobaseEftoXuMultipliers_df |> 
    # Conditional if mutate to avoid putting 100% where efficiency is 0; which corresponds to a missing efficiency anyway.
    # However we are now filtering out 0 efficiencies in the calc_Ef_to_Eu_exiobase() function to help identify potentially missing efficiencies
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.double), ~ dplyr::case_when(.x == 0 ~ .x, 
                                             .x >= 1 ~ 0,
                                             TRUE ~ 1 - .x)
      )
    )
  return(Ef_to_Xloss_multipliers)
}
