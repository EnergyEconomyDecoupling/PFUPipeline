#' Load FU allocation tables
#'
#' This function reads all final-to-useful allocation data
#' in files in the `fu_analysis_folder` that start with the country abbreviations
#' given in `countries`.
#'
#' By default, it is assumed that each country's final-to-useful analysis file will be in a subfolder
#' of `fu_analysis_path`.
#' Set `use_subfolders` to `FALSE` to change the default behavior.
#'
#' If final-to-useful allocation data are not available, this function
#' automatically creates an empty final-to-useful allocation template and writes it to disk.
#' Then, this function reads the empty file.
#' This behavior can be modified by setting argument `generate_missing_fu_allocation_template` to `FALSE`.
#'
#' @param fu_analysis_folder The folder from which final-to-useful analyses will be loaded.
#' @param specified_iea_data A data frame of specified IEA data for `countries`.
#' @param countries The countries for which allocation tables should be loaded.
#' @param file_suffix The suffix for the FU analysis files. Default is "`r IEATools::fu_analysis_file_info$fu_analysis_file_suffix`".
#' @param use_subfolders Tells whether to look for files in subfolders named by `countries`. Default is `TRUE`.
#' @param generate_missing_fu_allocation_template Tells whether to generate a missing final-to-useful allocation template from `specified_iea_data`. Default is `TRUE`.
#' @param fu_allocations_tab_name The name of the tab for final-to-useful allocations in the Excel file containing final-to-useful allocation data. Default is "`r IEATools::fu_analysis_file_info$fu_allocation_tab_name`".
#'
#' @export
#'
#' @return A data frame of FU Allocation tables read by `IEATools::load_fu_allocation_data()`.
#'         If no FU Allocation data are found and `generate_missing_fu_allocation_template` is `TRUE`,
#'         an empty template written to disk and the empty template is returned.
#'         If no FU Allocation data are found and `generate_missing_fu_allocation_template` is `FALSE`,
#'         `NULL` is returned.
load_fu_allocation_tables <- function(fu_analysis_folder,
                                      specified_iea_data,
                                      countries,
                                      file_suffix = IEATools::fu_analysis_file_info$fu_analysis_file_suffix,
                                      use_subfolders = TRUE,
                                      generate_missing_fu_allocation_template = TRUE,
                                      fu_allocations_tab_name = IEATools::fu_analysis_file_info$fu_allocation_tab_name) {
  out <- lapply(countries, FUN = function(coun) {
    folder <- ifelse(use_subfolders, file.path(fu_analysis_folder, coun), fu_analysis_folder)
    fpath <- file.path(folder, paste0(coun, file_suffix))
    fexists <- file.exists(fpath)
    if (!fexists & !generate_missing_fu_allocation_template) {
      return(NULL)
    }
    if (!fexists & generate_missing_fu_allocation_template) {
      # Make sure we have the folder we need
      dir.create(folder, showWarnings = FALSE)
      # Create and write the template
      iea_data <- specified_iea_data %>%
        dplyr::filter(.data[[IEATools::iea_cols$country]] == coun)
      IEATools::fu_allocation_template(iea_data) %>%
        IEATools::write_fu_allocation_template(fpath)
    }
    # Read the FU allocation data from fpath.
    IEATools::load_fu_allocation_data(fpath, fu_allocations_tab_name = fu_allocations_tab_name)
  }) %>%
    dplyr::bind_rows()
  if (nrow(out) == 0) {
    return(NULL)
  }
  return(out)
}