#' Add allocation matrices to a data frame
#'
#' This function adds allocation matrices (`C_Y` and `C_EIOU`) to the previously-created
#' `CompletedAllocationTables` target.
#'
#' @param completed_allocation_tables The completed allocation tables from which allocation (`C`) matrices should be created.
#'                                    This data frame is most likely to be the `CompletedAllocationTables` target.
#' @param countries The countries for which `C` matrices should be formed.
#' @param matrix_class The type of matrix that should be produced. 
#'                     One of "matrix" (the default and not sparse) or "Matrix" (which may be sparse).
#' @param country,year See `IEATools::iea_cols`.
#' @param c_source,.values,C_Y,C_EIOU See `IEATools::template_cols`.
#'
#' @return A data frame with `C_Y` and `C_EIOU` columns containing allocation matrices.
#'
#' @export
calc_C_mats <- function(completed_allocation_tables,
                        countries,
                        matrix_class = c("matrix", "Matrix"),
                        country = IEATools::iea_cols$country,
                        year = IEATools::iea_cols$year,
                        c_source = IEATools::template_cols$c_source,
                        .values = IEATools::template_cols$.values,
                        C_Y = IEATools::template_cols$C_Y,
                        C_EIOU  = IEATools::template_cols$C_eiou) {
  matrix_class <- match.arg(matrix_class)
  tables <- completed_allocation_tables %>%
    dplyr::filter(.data[[country]] %in% countries) %>%
    dplyr::mutate(
      # Eliminate the c_source column (if it exists) before sending
      # the completed_allocation_tables into form_C_mats().
      # The c_source column applies to individual C values, and we're making matrices out of them.
      # In other words, form_C_mats() doesn't know what to do with that column.
      "{c_source}" := NULL
    )
  # Need to form C matrices from completed_allocation_tables.
  # Use the IEATools::form_C_mats() function for this task.
  # The function accepts a tidy data frame in addition to wide-by-year data frames.
  IEATools::form_C_mats(tables, matvals = .values, matrix_class = matrix_class)
}


#' Add efficiency (`eta`) and exergy-to-energy ratio (`phi`) vectors
#' to a data frame.
#'
#' This function adds final-to-useful efficiency (`eta`) and
#' exergy-to-energy ratio vectors to the previously-created `WithCmats` target.#'
#'
#' @param completed_efficiency_tables The completed efficiency tables from which efficiency (`eta_fu`) vectors
#'                                    should be created. This data frame is most likely to be the `CompletedEfficiencyTables` target.
#' @param completed_phi_tables The completed phi tables from which exergy-to-energy ratio vectors (`phi_u`)
#'                             should be created.This data frame is most likely to be the `CompletedPhiTables` target.
#' @param countries The countries for which `eta_fu` and `phi_u` vectors should be formed.
#' @param matrix_class The type of matrix that should be produced. 
#'                     One of "matrix" (the default and not sparse) or "Matrix" (which may be sparse).
#' @param country,year See `IEATools::ieacols`.
#' @param c_source,eta_fu_source,.values,eta_fu,phi_u See `IEATools::template_cols`.
#' @param phi_u_source See `IEATools::phi_constants_names`.
#'
#' @return A data frame with `eta_fu` and `phi_u` vectors added as columns.
#'
#' @export
calc_eta_fu_phi_u_vecs <- function(completed_efficiency_tables,
                                   completed_phi_tables,
                                   countries,
                                   matrix_class = c("matrix", "Matrix"),
                                   country = IEATools::iea_cols$country,
                                   year = IEATools::iea_cols$year,
                                   c_source = IEATools::template_cols$c_source,
                                   eta_fu_source = IEATools::template_cols$eta_fu_source,
                                   .values = IEATools::template_cols$.values,
                                   eta_fu = IEATools::template_cols$eta_fu,
                                   phi_u = IEATools::template_cols$phi_u,
                                   phi_u_source = IEATools::phi_constants_names$phi_source_colname) {
  matrix_class <- match.arg(matrix_class)
  lapply(list(completed_efficiency_tables, completed_phi_tables), function(t) {
    t %>%
      dplyr::filter(.data[[country]] %in% countries) %>%
      dplyr::mutate(
        # Eliminate the c_source, eta_fu_source, and phi_u_source columns
        # (if they exists) before sending
        # the completed_allocation_tables into form_eta_fu_phi_u_vecs().
        # The c_source, eta_fu_source, and phi_u_source columns
        # apply to individual eta_fu and phi_u values, and
        # we're making vectors out of them.
        # In other words, form_eta_fu_phi_u_vecs() doesn't
        # know what to do with those columns.
        "{c_source}" := NULL,
        "{eta_fu_source}" := NULL,
        "{phi_u_source}" := NULL
      )
  }) %>%
    dplyr::bind_rows() %>%
    # Need to form eta_fu and phi_u vectors from completed_efficiency_tables.
    # Use the IEATools::form_eta_fu_phi_u_vecs() function for this task.
    # The function accepts a tidy data frame in addition to wide-by-year data frames.
    IEATools::form_eta_fu_phi_u_vecs(matvals = .values, matrix_class = matrix_class)
}


#' Choose eta.fu or phi.u columns from a data frame of eta.fu and phi.u vectors.
#'
#' @param eta_fu_phi_u_vecs A data frame containing metadata columns and columns for
#'                          eta_fu (final to useful efficiency) and
#'                          phi_u (exergy-to-energy efficiency ratios).
#' @param keep Tells which column to keep, eta_fu or phi_u.
#'             Must be one of `IEATools::template_cols$eta_fu` or `IEATools::template_cols$phi_u`.
#' @param countries The countries to be analyzed.
#' @param country See IEATools::iea_cols.
#'
#' @return A data frame of metadata and either an eta_fu column or a phi_u column
#'
#' @export
sep_eta_fu_phi_u <- function(eta_fu_phi_u_vecs,
                             keep = c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u),
                             countries,
                             country = IEATools::iea_cols$country) {
  keep = match.arg(keep, several.ok = FALSE)
  out <- eta_fu_phi_u_vecs %>%
    dplyr::filter(.data[[country]] %in% countries)
  if (keep == IEATools::template_cols$eta_fu) {
    col_to_delete = IEATools::template_cols$phi_u
  } else {
    col_to_delete = IEATools::template_cols$eta_fu
  }
  out %>%
    dplyr::mutate(
      "{col_to_delete}" := NULL
    )
}


#' Move the last stage of the energy conversion chain from final stage to useful stage with details
#'
#' Extends the energy conversion chain from a final energy last stage to useful energy last stage.
#' Details about the conversion from final to useful are retained via matrices
#' **Y_fu_detailed** and **U_EIOU_fu_detailed**.
#' The last-stage-useful energy conversion chain PSUT matrices are bound as rows at the bottom
#' of the `psut_final` data frame,
#' albeit with "Useful" instead of "Final" in the `Last.stage` column.
#'
#' @param psut_final A data frame with rows that describe the energy conversion chain with final energy as the last stage.
#' @param C_mats A data frame with allocation matrices, probably the Cmats target.
#' @param eta_phi_vecs A data frame with final-to-useful efficiency and exergy-to-energy ratio vectors.
#' @param countries The countries to be analyzed.
#' @param country,year See IEATools::iea_cols.
#' @param C_Y,C_eiou See IEATools::template_cols.
#'
#' @return A data frame with energy conversion chain matrices with last stage as useful energy.
#'
#' @export
move_to_useful_with_details <- function(psut_final,
                                        C_mats,
                                        eta_phi_vecs,
                                        countries,
                                        country = IEATools::iea_cols$country,
                                        year = IEATools::iea_cols$year,
                                        C_Y = IEATools::template_cols$C_Y,
                                        C_eiou = IEATools::template_cols$C_eiou) {
  
  # Calculate metadata columns.
  m_cols <- C_mats %>%
    IEATools::meta_cols(return_names = TRUE,
                        years_to_keep = year,
                        not_meta = c(C_Y, C_eiou))
  psut_final_filtered <- psut_final |> 
    dplyr::filter(.data[[country]] %in% countries)
    
  psut_final_filtered %>%
    # Join the matrices and vectors to the psut_final data frame.
    dplyr::full_join(C_mats %>% dplyr::filter(.data[[country]] %in% countries), by = m_cols) %>%
    dplyr::full_join(eta_phi_vecs %>% dplyr::filter(.data[[country]] %in% countries), by = m_cols) %>%
    # And, finally, extend to the useful stage.
    IEATools::extend_to_useful() |> 
    IEATools::stack_final_useful_df(psut_final_filtered)
}


#' Remove columns from the PSUTUsefulIEAWithDetails target
#' 
#' A simple wrapper function to assist with removing
#' unneeded columns from the `PSUTUsefulIEAWithDetails` target.
#'
#' This function enables mapping over countries.
#'
#' @param psut_useful_iea_with_details The target from which columns should be removed.
#' @param cols_to_remove A string vector of columns names to be removed.
#' @param remove_final A boolean that tells whether to remove Last.stage == "Final" rows.
#'                     Because the detail matrices contain useful-stage data,
#'                     there is, typically, no need to keep the useful rows.
#'                     Default is `TRUE`.
#' @param countries The countries for which this function should be applied.
#' @param country The name of the `Country` column in `psut_useful_iea_with_details` and `phi_vecs`.
#'                Default is `IEATools::iea_cols$country`.
#' @param year The name of the `Year` column in  `psut_useful_iea_with_details` and `phi_vecs`.
#'             Default is `IEATools::iea_cols$year`.
#' @param last_stage The name of the "Last.stage" column. 
#'                   Default is `IEATools::iea_cols$last_stage`.
#' @param final The string defining the final stage.
#'              Default is `IEATools::all_stages$final`.
#'
#' @return A version of `psut_useful_iea_with_details` with `cols_to_remove` removed.
#' 
#' @export
remove_cols_from_PSUTUsefulIEAWithDetails <- function(psut_useful_iea_with_details, 
                                                      cols_to_remove,
                                                      phi_vecs,
                                                      remove_final = TRUE,
                                                      countries, 
                                                      country = IEATools::iea_cols$country, 
                                                      year = IEATools::iea_cols$year,
                                                      last_stage = IEATools::iea_cols$last_stage, 
                                                      final = IEATools::all_stages$final) {
  out <- psut_useful_iea_with_details |> 
    dplyr::filter(.data[[country]] %in% countries) |> 
    dplyr::select(-dplyr::any_of(cols_to_remove))
  if (remove_final) {
    out <- out |> 
      dplyr::filter(.data[[last_stage]] != final)
  }
  return(out)
}


  
  
  
  
