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
#' @param iea_path the path to the IEA data directory, relative to `project_path`.
#' @param oecd_path the path to the OECD data file, relative to `iea_path`.
#' @param nonoecd_path the path to the non-OECD data file, relative to `iea_path`.
#'
#' @return a named list containing paths to important directories and files, including
#' `home_path` (the absolute path to the user's home),
#' `dropbox_path` (the absolute path of the user's Dropbox folder)
#' `project_path` (the absolute path to the project folder),
#' `iea_path` (the absolute path to a folder containing IEA data),
#' `oecd_path` (the absolute path to the IEA data file for the OECD countries), and
#' `nonoecd_path` (the absolute path to the IEA data file for non-OECD countries).
#' 
#' @export
#'
#' @examples
#' get_abs_paths()
get_abs_paths <- function(home_path = sub(pattern = "Documents$", replacement = "", x = file.path(Sys.getenv("HOME"))),
                          dropbox_path = "Dropbox",
                          project_path = file.path(dropbox_path, "Fellowship 1960-2015 PFU database"), 
                          iea_path = file.path(project_path, "IEA extended energy data", "IEA 2015 energy balance data"), 
                          oecd_path = file.path(iea_path, "energy-balances-oecd-extended-energy.csv"), 
                          nonoecd_path = file.path(iea_path, "energy-balances-nonoecd-extended-energy.csv")) {

  list(home_path = home_path,
       dropbox_path = file.path(home_path, dropbox_path),
       project_path = file.path(home_path, project_path), 
       iea_path = file.path(home_path, iea_path), 
       oecd_path = file.path(home_path, oecd_path), 
       nonoecd_path = file.path(home_path, nonoecd_path))
}