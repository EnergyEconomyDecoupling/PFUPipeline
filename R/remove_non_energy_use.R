#' Remove Non-energy use from energy conversion chains
#' 
#' It is helpful to remove Non-energy use from ECCs.
#' This function calls `Recca::remove_neu()`.
#'
#' @param .psut_mats A data frame of PSUT matrices.
#'
#' @return A version of `.psut_mats` with Non-energy use removed.
#' 
#' @export
remove_non_energy_use <- function(.psut_mats) {
  .psut_mats |> 
    Recca::remove_neu() |> 
    dplyr::mutate(
      # Eliminate unneeded columns
      R = NULL, 
      U = NULL, 
      U_feed = NULL, 
      U_EIOU = NULL, 
      r_EIOU = NULL, 
      V = NULL, 
      Y = NULL, 
      S_units = NULL
    ) |> 
    dplyr::rename(
      # Rename matrices without NEU ("_prime") to regular column names.
      R = "R_prime", 
      U = "U_prime",
      U_feed = "U_feed_prime",
      U_EIOU = "U_EIOU_prime", 
      r_EIOU = "r_EIOU_prime",
      V = "V_prime",
      Y = "Y_prime",
      S_units = "S_units_prime"
    )}