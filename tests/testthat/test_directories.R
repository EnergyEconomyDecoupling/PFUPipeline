###########################################################
context("Path functions")
###########################################################

test_that("Path functions work as expected", {
  my_home_path <- sub(pattern = "Documents$", replacement = "", x = file.path(Sys.getenv("HOME")))
  paths <- get_abs_paths()
  expect_equal(paths$home_path, my_home_path)
  expect_equal(paths$dropbox_path, file.path(my_home_path, "Dropbox"))
  expect_equal(paths$project_path, file.path(my_home_path, "Dropbox", "Fellowship 1960-2015 PFU database"))
  expect_equal(paths$iea_folder_path, file.path(my_home_path, "Dropbox", "Fellowship 1960-2015 PFU database", 
                                                "IEA extended energy balance data", 
                                                "IEA 2019 energy balance data"))
  expect_equal(paths$iea_data_path, file.path(my_home_path, "Dropbox", "Fellowship 1960-2015 PFU database", 
                                              "IEA extended energy balance data", 
                                              "IEA 2019 energy balance data", 
                                              "IEA Extended Energy Balances 2019.csv"))
  expect_equal(paths$fu_analysis_path, file.path(my_home_path, "Dropbox", "Fellowship 1960-2015 PFU database", 
                                                 "Country-level exergy accounting data"))
})
  