

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
  
  
  # (2) Calculations of eta * phi
  # Represents the final-to-useful energy efficiency times the useful stage Phi coefficient
  eta_fu_eff_phi_Y_EIOU_agg <- C_mats_agg_excl_NEU |> 
    dplyr::left_join(eta_fu_vecs, by = c({country}, {method}, {energy_type}, {last_stage}, {year})) |> 
    dplyr::left_join(phi_vecs, by = c({country}, {year})) |> 
    dplyr::mutate(
      "{eta_phi_p_eiou}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_agg]], matsbyname::hatize_byname(.data[[eta.fu]])) |> 
        matsbyname::clean_byname(margin = 1) |> 
        matsbyname::rename_to_piece_byname(piece = "suff", margin = 2, notation = list(RCLabels::arrow_notation)) |> 
        matsbyname::aggregate_byname(margin = 2) |> 
        matsbyname::setcoltype(product) |> 
        matsbyname::matrixproduct_byname(phi),
      "{eta_phi_p_y}" := matsbyname::matrixproduct_byname(.data[[C_Y_agg]], matsbyname::hatize_byname(.data[[eta.fu]])) |> 
        matsbyname::clean_byname(margin = 1) |> 
        matsbyname::rename_to_piece_byname(piece = "suff", margin = 2, notation = list(RCLabels::arrow_notation)) |> 
        matsbyname::aggregate_byname(margin = 2) |> 
        matsbyname::setcoltype(product) |> 
        matsbyname::matrixproduct_byname(phi),
      "{eta_phi_p_eiou_y}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_Y_agg]], matsbyname::hatize_byname(.data[[eta.fu]])) |> 
        matsbyname::clean_byname(margin = 1) |> 
        matsbyname::rename_to_piece_byname(piece = "suff", margin = 2, notation = list(RCLabels::arrow_notation)) |> 
        matsbyname::aggregate_byname(margin = 2) |> 
        matsbyname::setcoltype(product) |> 
        matsbyname::matrixproduct_byname(phi),
    ) |> 
    dplyr::select(.data[[country]], .data[[method]], .data[[year]], .data[[eta_phi_p_eiou]], .data[[eta_phi_p_y]], .data[[eta_phi_p_eiou_y]])
  
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
#'
#' @return A data frame of final energy to final exergy multipliers
#'
#' @export
calc_Ef_to_Xf_exiobase <- function(phi_vecs,
                                   years_exiobase,
                                   full_list_exiobase_flows,
                                   country_concordance_table_df) {
  
  # Filtering out non final energy flows
  # We keep losses and non-energy uses in the multipliers we produce for Exiobase
  list_final_energy_flows <- full_list_exiobase_flows %>%
    dplyr::filter(Final.energy.flow == TRUE) %>%
    dplyr::select(Exiobase.Flow)
  
  # Expanding to determine phi values for each Exiobase flow
  phi_vals_df <- phi_vecs %>%
    dplyr::filter(Year %in% years_exiobase) %>%
    tidyr::pivot_longer(cols = phi, names_to = "matnames", values_to = "matvals") %>%
    matsindf::expand_to_tidy(rownames = "Product") %>%
    dplyr::select(-matnames, -colnames, -rowtypes, -coltypes) %>%
    tidyr::expand_grid(list_final_energy_flows) %>%
    dplyr::filter(Product %in% IEATools::products) %>%
    dplyr::rename(PFU.code = Country) %>%
    dplyr::left_join(country_concordance_table_df %>% dplyr::select(IEA.name.accented, PFU.code),
                     by = "PFU.code") %>%
    dplyr::select(-PFU.code) %>%
    dplyr::rename(IEA.country.name = IEA.name.accented,
                  Flow = Exiobase.Flow) %>%
    dplyr::filter(!is.na(IEA.country.name)) %>%
    dplyr::relocate(IEA.country.name, .before = Year) %>%
    tidyr::pivot_wider(names_from = Year, values_from = matvals)
  
  return(phi_vals_df)
}


#' Calculates the final energy to useful energy multipliers
#'
#' @param eta_fu_Y_EIOU_mats The input data frame containing matrices with all the efficiencies by final demand sector and energy industry
#' @param eta_fu_Y_EIOU_agg The input data frame containing matrices with the economy-wide efficiencies by energy product
#' @param years_exiobase The years for which the coefficients are provided to the Exiobase team
#' @param full_list_exiobase_flows The full list of energy flows used in the Exiobase pipeline led by KR
#' @param country_concordance_table_df A data frame containing the country concordance table
#'
#' @return A dataframe of the final energy to useful energy multipliers
#' @export
calc_Ef_to_Eu_exiobase <- function(eta_fu_Y_EIOU_mats,
                                   eta_fu_Y_EIOU_agg,
                                   years_exiobase,
                                   full_list_exiobase_flows,
                                   country_concordance_table_df){
  
  # Filtering out non useful energy flows
  # So here we remove non-energy uses and losses
  list_useful_energy_flows <- full_list_exiobase_flows %>%
    dplyr::filter(Useful.energy.flow == TRUE) %>%
    dplyr::select(Exiobase.Flow, PFU.flow)
  
  # Expanding the EIOU-wide efficiencies data frame to prepare the join
  eta_fu_EIOU_wide_df <- eta_fu_Y_EIOU_agg |> 
    dplyr::filter(Energy.type == "E") |> 
    dplyr::select(Country, Method, Energy.type, Year, eta_p_eiou) |> 
    tidyr::pivot_longer(cols = eta_p_eiou, values_to = "matvals", names_to = "matnames") |> 
    matsindf::expand_to_tidy() |> 
    dplyr::rename(
      Product = rownames,
      eta = "matvals"
    ) |> 
    dplyr::select(Country, Method, Energy.type, Year, Product, eta) |>
    dplyr::mutate(PFU.flow = "EIOU-wide",
                  Last.stage = "Final")
  
  # Expanding the final-to-useful efficiencies to prepare the join
  eta_fu_df <- eta_fu_Y_EIOU_mats |>
    dplyr::filter(Year %in% years_exiobase) |> 
    dplyr::select(Country:Year, eta_fu_Y_E:eta_fu_EIOU_E) |> 
    tidyr::pivot_longer(cols = tidyr::ends_with("_E"), names_to = "matnames", values_to = "matvals") |> 
    matsindf::expand_to_tidy(rownames = "Product", colnames = "PFU.flow") |> 
    dplyr::select(-rowtypes, -coltypes, -matnames) |> 
    dplyr::rename(eta = matvals) |> 
    dplyr::filter(Product %in% IEATools::products) |>
    dplyr::bind_rows(eta_fu_EIOU_wide_df) |> 
    dplyr::filter(eta != 0)
  
  # Expanding the economy-wide efficiencies data frame to prepare the join
  eta_fu_economy_wide_df <- eta_fu_Y_EIOU_agg |> 
    dplyr::filter(Energy.type == "E") |> 
    dplyr::select(Country, Method, Energy.type, Year, eta_p_eiou_y) |> 
    tidyr::pivot_longer(cols = eta_p_eiou_y, values_to = "matvals", names_to = "matnames") |> 
    matsindf::expand_to_tidy() |> 
    dplyr::rename(
      Product = rownames,
      PFU.code = Country,
      eta = "matvals"
    ) |> 
    dplyr::select(PFU.code, Method, Energy.type, Year, Product, eta) |>
    dplyr::mutate(Exiobase.Flow = "Economy-wide") |> 
    dplyr::filter(eta != 0)
  
  # Preparing the (Country, Year, Product) list
  country_year_product_list <- eta_fu_df |> 
    dplyr::select(Country, Year, Product) |> 
    dplyr::distinct()
  
  # List of Exiobase flows to be joined to the PFU results
  expanded_exiobase_Ue_flows <- list_useful_energy_flows |> 
    tidyr::expand_grid(country_year_product_list) |> 
    dplyr::relocate(Exiobase.Flow:PFU.flow, .after = Year)
  
  # Now, joining to ascribe each Exiobase flow a PFU flow
  Ef_to_Eu_multipliers <- expanded_exiobase_Ue_flows |> 
    dplyr::left_join(eta_fu_df, by = c("Country", "Year", "Product", "PFU.flow")) |> 
    dplyr::filter(!is.na(eta)) |> 
    dplyr::rename(PFU.code = Country) |> 
    dplyr::select(-PFU.flow) |> 
    dplyr::bind_rows(eta_fu_economy_wide_df) |> 
    dplyr::left_join(country_concordance_table_df %>% dplyr::select(IEA.name.accented, PFU.code),
                     by = "PFU.code") |>
    dplyr::select(-PFU.code) |>
    dplyr::rename(IEA.country.name = IEA.name.accented,
                  Flow = Exiobase.Flow) |>
    dplyr::filter(!is.na(IEA.country.name)) |>
    dplyr::select(IEA.country.name, Year, Product, Flow, eta) |>
    dplyr::relocate(Flow, .before = Product) |>
    tidyr::pivot_wider(names_from = Year, values_from = eta)
  
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
        where(is.double), ~ dplyr::case_when(.x == 0 ~ .x, 
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
#'
#' @return A data frame of the final energy to useful exergy multipliers
#' @export
calc_Ef_to_Xu_exiobase <- function(EtafuYEIOU_mats,
                                   phi_vecs,
                                   eta_fu_phi_Y_EIOU_agg,
                                   years_exiobase,
                                   full_list_exiobase_flows,
                                   country_concordance_table_df){
  
  # Filtering out non useful energy flows
  # So here we remove non-energy uses and losses
  list_useful_energy_flows <- full_list_exiobase_flows %>%
    dplyr::filter(Useful.energy.flow == TRUE) %>%
    dplyr::select(Exiobase.Flow, PFU.flow)
  
  # Expanding the EIOU-wide efficiencies*phi values data frame to prepare the join
  eta_times_phi_EIOU_wide_df <- eta_fu_phi_Y_EIOU_agg |> 
    dplyr::select(Country, Method, Year, eta_phi_p_eiou) |> 
    tidyr::pivot_longer(cols = eta_phi_p_eiou, values_to = "matvals", names_to = "matnames") |> 
    matsindf::expand_to_tidy() |> 
    dplyr::rename(
      Product = rownames,
      phi_eta_X = "matvals"
    ) |> 
    dplyr::select(Country, Method, Year, Product, phi_eta_X) |>
    dplyr::mutate(PFU.flow = "EIOU-wide",
                  Last.stage = "Final",
                  Energy.type = "E")
  
  # Expanding Phivecs
  phi_vals_df <- phi_vecs |> 
    dplyr::filter(Year %in% years_exiobase) |> 
    tidyr::pivot_longer(cols = phi, names_to = "matnames", values_to = "matvals") |> 
    matsindf::expand_to_tidy(rownames = "Product") |> 
    dplyr::select(-matnames, -colnames, -rowtypes, -coltypes) |> 
    dplyr::rename(PFU.code = Country,
                  phi = matvals)
  
  # Expanding the final-to-useful efficiencies to prepare the join
  phi_eta_fu_df <- EtafuYEIOU_mats |>
    dplyr::filter(Year %in% years_exiobase) |> 
    dplyr::select(Country:Year, eta_fu_Y_X:eta_fu_EIOU_X) |> 
    tidyr::pivot_longer(cols = tidyr::ends_with("_X"), names_to = "matnames", values_to = "matvals") |> 
    matsindf::expand_to_tidy(rownames = "Product", colnames = "PFU.flow") |> 
    dplyr::select(-rowtypes, -coltypes, -matnames) |> 
    dplyr::rename(eta = matvals) |> 
    dplyr::filter(Product %in% IEATools::products) |>
    dplyr::left_join(phi_vals_df |> dplyr::rename(Country = PFU.code), by = c("Country", "Year", "Product")) |> 
    dplyr::mutate(
      phi_eta_X = phi * eta
    ) |> 
    dplyr::select(-eta, -phi) |> 
    dplyr::bind_rows(eta_times_phi_EIOU_wide_df) |> 
    dplyr::filter(phi_eta_X != 0)
  
  # Expanding the economy-wide efficiencies*phi values data frame to prepare the join
  eta_times_phi_economy_wide_df <- eta_fu_phi_Y_EIOU_agg |> 
    dplyr::select(Country, Method, Year, eta_phi_p_eiou_y) |> 
    tidyr::pivot_longer(cols = eta_phi_p_eiou_y, values_to = "matvals", names_to = "matnames") |> 
    matsindf::expand_to_tidy() |> 
    dplyr::rename(
      Product = rownames,
      PFU.code = Country,
      phi_eta_X = "matvals"
    ) |> 
    dplyr::select(PFU.code, Method, Year, Product, phi_eta_X) |>
    dplyr::mutate(Exiobase.Flow = "Economy-wide") |> 
    dplyr::filter(phi_eta_X != 0)
  
  # Preparing the (Country, Year, Product) list
  country_year_product_list <- phi_eta_fu_df |> 
    dplyr::select(Country, Year, Product) |> 
    dplyr::distinct()
  
  # List of Exiobase flows to be joined to the PFU results
  expanded_exiobase_Ue_flows <- list_useful_energy_flows |> 
    tidyr::expand_grid(country_year_product_list) |> 
    dplyr::relocate(Exiobase.Flow:PFU.flow, .after = Year)
  
  # Now, joining to ascribe each Exiobase flow a PFU flow
  Ef_to_Xu_multipliers <- expanded_exiobase_Ue_flows |> 
    dplyr::left_join(phi_eta_fu_df, by = c("Country", "Year", "Product", "PFU.flow")) |> 
    dplyr::filter(!is.na(phi_eta_X)) |> 
    dplyr::rename(PFU.code = Country) |> 
    dplyr::select(-PFU.flow) |> 
    dplyr::bind_rows(eta_times_phi_economy_wide_df) |> 
    dplyr::left_join(country_concordance_table_df %>% dplyr::select(IEA.name.accented, PFU.code),
                     by = "PFU.code") |>
    dplyr::rename(IEA.country.name = IEA.name.accented,
                  Flow = Exiobase.Flow) |>
    dplyr::filter(!is.na(IEA.country.name)) |>
    dplyr::select(IEA.country.name, Year, Product, Flow, phi_eta_X) |>
    dplyr::relocate(Flow, .before = Product) |>
    tidyr::pivot_wider(names_from = Year, values_from = phi_eta_X)
  
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
        where(is.double), ~ dplyr::case_when(.x == 0 ~ .x, 
                                             .x >= 1 ~ 0,
                                             TRUE ~ 1 - .x)
      )
    )
  return(Ef_to_Xloss_multipliers)
}
