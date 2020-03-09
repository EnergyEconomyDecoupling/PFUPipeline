#' Create a final-to-useful allocation template
#'
#' @param country a string of the 3-letter ISO country code
#' @param file_name the file name for the template. Default is "FU Allocations <<3-letter country code>>".
#' @param ext the file name extension. Default is ".xlsx".
#' @param data_target the data target from which the template is created. Default is "Specified".
#' @param paths_target the name of the paths object. Default is "paths".
#' @param fu_analysis_path_name the name of the final-to-useful path member of the paths object. Default is "fu_analysis_path".
#' @param overwrite tells whether to overwrite existing file. Default is `FALSE`.
#'
#' @return the path to the final-to-useful analysis template
#' 
#' @export
generate_fu_allocation_template <- function(country,
                                            data_target = "Specified",
                                            paths_target = "paths",
                                            fu_analysis_path_name = "fu_analysis_path",
                                            file_name = paste0("FU Allocations ", country),
                                            ext = ".xlsx", 
                                            overwrite = FALSE) {
  # Construct the output path from the FU analysis folder and file_name.
  output_path <- file.path(readd(paths_target, character_only = TRUE)[[fu_analysis_path_name]], 
                           paste0(file_name, ext))
  # Get the specified data for this country
  readd_by_country(data_target, country) %>%
    # Create the allocation template
    fu_allocation_template() %>%
    # Write the allocation template
    write_fu_allocation_template(output_path, overwrite = overwrite)
}


generate_eta_fu_template <- function(country, 
                                     fu_allocation_file) {
  
}