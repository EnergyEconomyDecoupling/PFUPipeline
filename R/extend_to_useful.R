#' Add allocation matrices to a data frame
#'
#' This function adds allocation matrices (`C_Y` and `C_EIOU`) to the previously-created
#' `CompletedAllocationTables` target.
#'
#' @param completed_allocation_tables The completed allocation tables from which allocation (`C`) matrices should be created.
#'                                    This data frame is most likely to be the `CompletedAllocationTables` target.
#' @param countries The countries for which `C` matrices should be formed
#' @param country,year See `IEATools::ieacols`.
#' @param c_source,.values,C_Y,C_EIOU See `IEATools::template_cols`.
#'
#' @return A data frame with `C_Y` and `C_EIOU` columns containing allocation matrices.
#'
#' @export
calc_C_mats <- function(completed_allocation_tables,
                        countries,
                        country = IEATools::iea_cols$country,
                        year = IEATools::iea_cols$year,
                        c_source = IEATools::template_cols$c_source,
                        .values = IEATools::template_cols$.values,
                        C_Y = IEATools::template_cols$C_Y,
                        C_EIOU  = IEATools::template_cols$C_eiou) {
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
  IEATools::form_C_mats(tables, matvals = .values)
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
                                   country = IEATools::iea_cols$country,
                                   year = IEATools::iea_cols$year,
                                   c_source = IEATools::template_cols$c_source,
                                   eta_fu_source = IEATools::template_cols$eta_fu_source,
                                   .values = IEATools::template_cols$.values,
                                   eta_fu = IEATools::template_cols$eta_fu,
                                   phi_u = IEATools::template_cols$phi_u,
                                   phi_u_source = IEATools::phi_constants_names$phi_source_colname) {
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
    IEATools::form_eta_fu_phi_u_vecs(matvals = .values)
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
