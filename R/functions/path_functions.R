# Your custom code is a bunch of functions.

#' Get absolute paths to important files and folders appropriate for this computer
#'
#' Default argument values assume a directory structure that includes Dropbox
#' and is appropriate for the Fellowship project for Dr. Paul Brockway.
#'
#' The default argument for `home_path` gets the value for `Sys.getenv("HOME")`.
#' On Windows, user directories reported by Sys.getenv("HOME") are set to the user's "Documents" folder by default.
#' `R` on other OSs does not append the "Documents" folder at the end of the `home_path`.
#' For the `home_path` argument,
#' we want the path without the Documents folder appended, because that is the enclosing directory
#' for the Dropbox folder's default location.
#' Thus, the default argument for `home_path` trims trailing "Documents", if present.
#' The default value for the `home_path` argument won't find "Documents" at the end of macOS and Linux home paths
#' and will return the HOME path, as desired.
#'
#' @param home_path the absolute path to the user's home directory.
#' @param dropbox_path the path to the user's Dropbox directory, relative to `home_path`.
#' @param project_path the path to the project directory, relative to `dropbox_path`.
#' @param iea_folder_path the path to the IEA data directory, relative to `project_path`.
#' @param iea_data_path the path to the IEA data file, relative to `iea_folder_path`.
#' @param exemplar_table_path The path to the exemplar table, relative to `home_path`.
#' @param fu_analysis_folder the path to the folder containing final-to-useful exergy information, relative to `home_path`.
#'
#' @return a named list containing paths to important directories and files, including
#' `home_path` (the absolute path to the user's home),
#' `dropbox_path` (the absolute path of the user's Dropbox folder)
#' `project_path` (the absolute path to the project folder),
#' `iea_folder_path` (the absolute path to a folder containing IEA data),
#' `iea_data_path` (the absolute path to the IEA data file for the OECD countries), and
#' `fu_analysis_path` (the absolute path to the folder containing final-to-useful exergy information).
#'
#' @export
#'
#' @examples
#' get_abs_paths()
get_abs_paths <- function(home_path = sub(pattern = "Documents$", replacement = "", x = file.path(Sys.getenv("HOME"))),
                          dropbox_path = "Dropbox",
                          project_path = file.path(dropbox_path,
                                                   "Fellowship 1960-2015 PFU database"),
                          iea_folder_path = file.path(project_path,
                                                      "IEA extended energy balance data",
                                                      "IEA 2019 energy balance data"),
                          iea_data_path = file.path(iea_folder_path,
                                                    "IEA Extended Energy Balances 2019.csv"),
                          fu_analysis_folder = file.path(project_path, "Country-level exergy accounting data"), 
                          exemplar_table_path = file.path(project_path, "Database plan", "Exemplar_Table.xlsx")) {

  list(home_path = home_path,
       dropbox_path = file.path(home_path, dropbox_path),
       project_path = file.path(home_path, project_path),
       iea_folder_path = file.path(home_path, iea_folder_path),
       iea_data_path = file.path(home_path, iea_data_path), 
       exemplar_table_path = file.path(home_path, exemplar_table_path), 
       fu_analysis_folder = file.path(home_path, fu_analysis_folder))
}
