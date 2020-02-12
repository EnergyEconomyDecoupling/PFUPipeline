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
#' @export
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

#' Get path to IEA subfolder inside data folder
#' 
#' If the IEA data are included in the data_folder directly, `iea_relpath` can be set to the empty string `""`.
#' 
#' @param data_folder path to the data folder
#' @param iea_relpath the relative path to the IEA data folder inside `data_folder`
#'
#' @return the full path to the IEA data folder inside `data_folder`
#' 
#' @export
#'
#' @examples
#' iea_subfolder(file.path(get_paths()[["data_path"]], "Fellowship 1960-2015 PFU database"))
#' iea_subfolder(file.path(get_paths()[["data_path"]], "Fellowship 1960-2015 PFU database"), "")
iea_subfolder <- function(data_folder, iea_relpath = file.path("IEA extended energy data", "IEA 2015 energy balance data")) {
  file.path(data_folder, iea_relpath)
}

#' Get path to file containing OECD data
#'
#' @param iea_folder the path to the IEA data folder
#' @param oecd_filename the name of the OECD data file inside `iea_folder`
#'
#' @return the full path to the IEA data for OECD countries
#' 
#' @export
#'
#' @examples
#' oecd_path(get_paths()[["iea_path"]])
oecd_path <- function(iea_folder, oecd_filename = "energy-balances-oecd-extended-energy.csv") {
  file.path(iea_folder, oecd_filename)
}

#' Get path to file containing non-OECD data
#' 
#' @param iea_folder the path to the IEA data folder
#' @param nonoecd_filename the name of the non-OECD data file inside `iea_folder`
#'
#' @return the full path to the IEA data for non-OECD countries
#' 
#' @export
#'
#' @examples
#' nonoecd_path(get_paths()[["iea_path"]])
nonoecd_path <- function(iea_folder, nonoecd_filename = "energy-balances-nonoecd-extended-energy.csv") {
  file.path(iea_folder, nonoecd_filename)
}
