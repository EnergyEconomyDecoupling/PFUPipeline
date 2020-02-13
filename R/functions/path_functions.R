# Your custom code is a bunch of functions.

#' Get absolute paths to important files and folders appropriate for this computer
#' 
#' Default argument values assume a directory structure that includes Dropbox.
#'
#' @return a named list containing paths to important directories and files, including
#' `home_path` (the absolute path to the user's home),
#' `dropbox_path` (the absolute path the the user's Dropbox folder)
#' `project_path` (the path to the project folder inside Dropbox),
#' `iea_path` (the path to a folder containing IEA data inside of `project_path`),
#' `oecd_path` (the path to the IEA data file for the OECD countries), and
#' `nonoecd_path` (the path to the IEA data file for non-OECD countries).
#' 
#' @export
#'
#' @examples
#' get_abs_paths()
get_abs_paths <- function(dropbox_path = "Dropbox",
                          project_path = file.path(dropbox_path, "Fellowship 1960-2015 PFU database"), 
                          iea_path = file.path(project_path, "IEA extended energy data", "IEA 2015 energy balance data"), 
                          oecd_path = file.path(iea_path, "energy-balances-oecd-extended-energy.csv"), 
                          nonoecd_path = file.path(iea_path, "energy-balances-nonoecd-extended-energy.csv")) {
  # On Windows, user directories reported by Sys.getenv("R_USER") are set to the Documents folder.
  # We want the path without the Documents folder appended.
  # This code won't find anything at the end of macOS and Linux home paths and will return the R_USER path.
  home_path <- sub(pattern = "Documents$", replacement = "", x = file.path(Sys.getenv("HOME")))
  
  list(home_path = home_path,
       dropbox_path = file.path(home_path, dropbox_path),
       project_path = file.path(home_path, project_path), 
       iea_path = file.path(home_path, iea_path), 
       oecd_path = file.path(home_path, oecd_path), 
       nonoecd_path = file.path(home_path, nonoecd_path))
}