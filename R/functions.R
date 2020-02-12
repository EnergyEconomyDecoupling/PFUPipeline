# Your custom code is a bunch of functions.

get_paths <- function() {
  home_folder <- file.path(Sys.getenv("HOME"))
  shared_folder <- file.path(home_folder, "Dropbox", "Fellowship 1960-2015 PFU database")
  iea_folder <- file.path(shared_folder, "IEA extended energy data", "IEA 2015 energy balance data")
  oecd_path <- file.path(iea_folder, "energy-balances-oecd-extended-energy.csv")
  nonoecd_path <- file.path(iea_folder, "energy-balances-nonoecd-extended-energy.csv")
  
  list(home_folder = home_folder, 
       shared_folder = shared_folder, 
       iea_folder = iea_folder, 
       oecd_path = oecd_path, 
       nonoecd_path = nonoecd_path)
}
