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

