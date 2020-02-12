# Your custom code is a bunch of functions.

#' Get paths to files and folders appropriate for this computer
#'
#' @return a named list containing paths to important directories and files, including
#' `home_folder` (the path to the user's home),
#' `data_folder` (the path to a shared directory containing IEA data),
#' `iea_folder` (the path to a folder containing IEA data inside of `data_folder`),
#' `oecd_path` (the path to the IEA data for the OECD countries), and
#' `nonoecd_path` (the path to the IEA data for non-OECD countries).
#'
#' @examples
#' get_paths()
get_paths <- function(home_path = file.path(Sys.getenv("HOME")), data_relpath = file.path("Dropbox", "Fellowship 1960-2015 PFU database")) {
  data_path <- file.path(home_path, data_relpath)
  iea_path <- iea_subfolder(data_path)

  list(home_path = home_path, 
       data_path = data_path, 
       iea_path = iea_subfolder(data_path), 
       oecd_path = oecd_path(iea_path), 
       nonoecd_path = nonoecd_path(iea_path))
}

iea_subfolder <- function(data_folder, iea_relpath = file.path("IEA extended energy data", "IEA 2015 energy balance data")) {
  file.path(data_folder, iea_relpath)
}

oecd_path <- function(iea_folder, oecd_filename = "energy-balances-oecd-extended-energy.csv") {
  file.path(iea_folder, oecd_filename)
}

nonoecd_path <- function(iea_folder, nonoecd_filename = "energy-balances-nonoecd-extended-energy.csv") {
  file.path(iea_folder, nonoecd_filename)
}
