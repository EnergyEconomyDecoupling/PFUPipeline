
# Reads the country concordance table, and keeps countries in the database
# 
# @param country_concordance_table_file The name of the file containing the counctry concordance table
# @param countries Contains the list of countries included in the run of the database
# 
# @return A data frame containing the country concordance table
# @export
# read_country_concordance_table <- function(country_concordance_table_file,
#                                            countries){
#   
#   # Reading and filtering country concordance table
#   country_concordance_table <- PFUDatabase::load_country_concordance_table(
#     country_concordance_path = country_concordance_table_file
#   ) %>%
#     dplyr::filter(PFU.code %in% countries)
#   
#   return(country_concordance_table)
# }


#' Reads the list of Exiobase energy flows
#'
#' @param path_to_exiobase_data Contains the path to the folder containing input data from Exiobase
#' @param list_energy_flows_file Contains the name of the file containing all the Exiobase energy flows
#'
#' @return A data frame containsing the list of Exiobase energy flows, and a boolean column stating whether the flow is a final energy flow or not
#' @export
read_list_exiobase_energy_flows <- function(path_to_exiobase_data,
                                            list_energy_flows_file){
  
  list_exiobase_energy_flows <- readr::read_csv(paste0(path_to_exiobase_data, list_energy_flows_file))
  
  return(list_exiobase_energy_flows)
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
    dplyr::left_join(country_concordance_table_df %>% dplyr::select(IEA.name, PFU.code),
                     by = "PFU.code") %>%
    dplyr::select(-PFU.code) %>%
    dplyr::rename(IEA.country.name = IEA.name,
                  Flow = Exiobase.Flow) %>%
    dplyr::filter(!is.na(IEA.country.name)) %>%
    dplyr::relocate(IEA.country.name, .before = Year) %>%
    dplyr::mutate(
      dplyr::case_when(
        IEA.country.name == "Curacao/Netherlands Antilles" ~ "Curaçao/Netherlands Antilles",
        IEA.country.name == "Cote d'Ivoire" ~ "Côte d'Ivoire",
        TRUE ~ IEA.country.name
      )
    ) %>%
    tidyr::pivot_wider(names_from = Year, values_from = matvals)
  
  return(phi_vals_df)
}


#' Calculates the final energy to useful energy multipliers
#'
#' @param EtafuYEIOU_df The input data frame containing matrices with all the efficiencies by final demand sector and energy industry
#' @param years_exiobase The years for which the coefficients are provided to the Exiobase team
#' @param full_list_exiobase_flows The full list of energy flows used in the Exiobase pipeline led by KR
#' @param country_concordance_table_df A data frame containing the country concordance table
#'
#' @return A dataframe of the final energy to useful energy multipliers
#' @export
calc_Ef_to_Eu_exiobase <- function(EtafuYEIOU_mats,
                                   years_exiobase,
                                   full_list_exiobase_flows,
                                   country_concordance_table_df){
  
  # Filtering out non useful energy flows
  # So here we remove non-energy uses and losses
  list_useful_energy_flows <- full_list_exiobase_flows %>%
    dplyr::filter(Useful.energy.flow == TRUE) %>%
    dplyr::select(Exiobase.Flow, PFU.flow)
  
  # Expanding the final-to-useful efficiencies to prepare the join
  eta_fu_df <- EtafuYEIOU_mats |>
    dplyr::filter(Year %in% years_exiobase) |> 
    dplyr::select(Country:Year, eta_fu_Y_E:eta_fu_EIOU_E) |> 
    tidyr::pivot_longer(cols = tidyr::ends_with("_E"), names_to = "matnames", values_to = "matvals") |> 
    matsindf::expand_to_tidy(rownames = "Product", colnames = "PFU.flow") |> 
    dplyr::select(-rowtypes, -coltypes, -matnames) |> 
    dplyr::rename(eta = matvals) |> 
    dplyr::filter(Product %in% IEATools::products) |>
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
    dplyr::left_join(country_concordance_table_df %>% dplyr::select(IEA.name, PFU.code),
                     by = "PFU.code") |>
    dplyr::select(-PFU.code) |>
    dplyr::rename(IEA.country.name = IEA.name,
                  Flow = Exiobase.Flow) |>
    dplyr::filter(!is.na(IEA.country.name)) |>
    dplyr::select(IEA.country.name, Year, Product, Flow, eta) |>
    dplyr::relocate(Flow, .before = Product) |>
    dplyr::mutate(
      dplyr::case_when(
        IEA.country.name == "Curacao/Netherlands Antilles" ~ "Curaçao/Netherlands Antilles",
        IEA.country.name == "Cote d'Ivoire" ~ "Côte d'Ivoire",
        TRUE ~ IEA.country.name
      )
    ) |> 
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
#' @param EtafuYEIOU_df The input data frame containing matrices with all the efficiencies by final demand sector and energy industry
#' @param phi_vecs A data frame of phi (exergy-to-energy ratio) coefficients.
#' @param years_exiobase The years for which the coefficients are provided to the Exiobase team
#' @param full_list_exiobase_flows The full list of energy flows used in the Exiobase pipeline led by KR
#' @param country_concordance_table_df A data frame containing the country concordance table
#'
#' @return A data frame of the final energy to useful exergy multipliers
#' @export
calc_Ef_to_Xu_exiobase <- function(EtafuYEIOU_mats,
                                   phi_vecs,
                                   years_exiobase,
                                   full_list_exiobase_flows,
                                   country_concordance_table_df){
  
  # Filtering out non useful energy flows
  # So here we remove non-energy uses and losses
  list_useful_energy_flows <- full_list_exiobase_flows %>%
    dplyr::filter(Useful.energy.flow == TRUE) %>%
    dplyr::select(Exiobase.Flow, PFU.flow)
  
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
    dplyr::filter(eta != 0) |> 
    dplyr::left_join(phi_vals_df |> dplyr::rename(Country = PFU.code), by = c("Country", "Year", "Product")) |> 
    dplyr::mutate(
      phi_eta_X = phi * eta
    ) |> 
    dplyr::select(-eta, -phi)
  
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
    dplyr::left_join(country_concordance_table_df %>% dplyr::select(IEA.name, PFU.code),
                     by = "PFU.code") |>
    dplyr::select(-PFU.code) |>
    dplyr::rename(IEA.country.name = IEA.name,
                  Flow = Exiobase.Flow) |>
    dplyr::filter(!is.na(IEA.country.name)) |>
    dplyr::select(IEA.country.name, Year, Product, Flow, phi_eta_X) |>
    dplyr::relocate(Flow, .before = Product) |>
    dplyr::mutate(
      dplyr::case_when(
        IEA.country.name == "Curacao/Netherlands Antilles" ~ "Curaçao/Netherlands Antilles",
        IEA.country.name == "Cote d'Ivoire" ~ "Côte d'Ivoire",
        TRUE ~ IEA.country.name
      )
    ) |> 
    tidyr::pivot_wider(names_from = Year, values_from = phi_eta_X)
  
  # # Expanding to determine phi values for each Exiobase flow
  # Ef_to_Xu_multipliers <- EtafuYEIOU_mats |> 
  #   dplyr::filter(Year %in% years_exiobase) |> 
  #   dplyr::select(Country:Year, eta_fu_Y_X:eta_fu_EIOU_X) |> 
  #   tidyr::pivot_longer(cols = tidyr::ends_with("_X"), names_to = "matnames", values_to = "matvals") |> 
  #   matsindf::expand_to_tidy(rownames = "Product", colnames = "PFU.flow") |> 
  #   dplyr::select(-rowtypes, -coltypes, -matnames) |> 
  #   dplyr::rename(eta = matvals) |> 
  #   dplyr::left_join(
  #     list_final_energy_flows, by = "PFU.flow"
  #   ) |> 
  #   # THIS HERE NEEDS TO BE SORTED OUT!!!!
  #   dplyr::rename(PFU.code = Country,
  #                 Flow = Exiobase.Flow) |>
  #   dplyr::filter(!is.na(Flow)) |> 
  #   # select(PFU.flow) |> 
  #   # distinct() |> print()
  #   dplyr::filter(Product %in% IEATools::products) |>
  #   dplyr::left_join(phi_vals_df, by = c("PFU.code", "Year", "Product")) |> 
  #   dplyr::mutate(
  #     Ef_to_Xu_Multiplier = eta * phi
  #   ) |> 
  #   dplyr::left_join(country_concordance_table_df %>% dplyr::select(IEA.name, PFU.code),
  #                    by = "PFU.code") |>
  #   dplyr::select(-PFU.code) |>
  #   dplyr::rename(IEA.country.name = IEA.name) |>
  #   dplyr::filter(!is.na(IEA.country.name)) |>
  #   dplyr::select(IEA.country.name, Year, Product, Flow, Ef_to_Xu_Multiplier) |>
  #   dplyr::relocate(IEA.country.name, .before = Year) |>
  #   dplyr::filter(Ef_to_Xu_Multiplier != 0) |> 
  #   dplyr::mutate(
  #     dplyr::case_when(
  #       IEA.country.name == "Curacao/Netherlands Antilles" ~ "Curaçao/Netherlands Antilles",
  #       IEA.country.name == "Cote d'Ivoire" ~ "Côte d'Ivoire",
  #       TRUE ~ IEA.country.name
  #     )
  #   ) |> 
  #   tidyr::pivot_wider(names_from = Year, values_from = Ef_to_Xu_Multiplier)
  
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




#' #' Just a debugging function to be removed
#' #'
#' #' @param phi_vecs aa
#' #' @param country_concordance_table_df bb
#' #'
#' #' @return Failed country joins
#' #' @export
#' debug_country_joins <- function(phi_vecs,
#'                                 country_concordance_table_df){
#'   
#'   failed_country_join <- phi_vecs %>%
#'     tidyr::pivot_longer(cols = phi, names_to = "matnames", values_to = "matvals") %>%
#'     matsindf::expand_to_tidy(rownames = "Product") %>%
#'     dplyr::select(-matnames, -colnames, -rowtypes, -coltypes) %>%
#'     dplyr::rename(PFU.code = Country) %>%
#'     dplyr::left_join(country_concordance_table_df %>% dplyr::select(IEA.name, PFU.code),
#'                      by = "PFU.code") %>%
#'     dplyr::filter(is.na(IEA.name))
#'   
#'   return(failed_country_join)
#' }