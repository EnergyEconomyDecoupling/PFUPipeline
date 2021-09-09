# Matt found this code helpful when debugging functions that sit in other packages.
# Simply copy this code into an untitled file inside RStudio for the package you want to debug. 
# Then set breakpoints where necessary.
# When done, don't forget to delete the .drake directory.


library(PFUSetup)
library(SEAPSUTWorkflow)

plan <- get_plan(countries = c("USA"),
                 additional_exemplar_countries = c("WLD"),
                 max_year = 2019,
                 iea_data_path = PFUSetup::get_abs_paths()[["iea_data_path"]],
                 country_concordance_path = PFUSetup::get_abs_paths()[["country_concordance_path"]],
                 ceda_data_folder = PFUSetup::get_abs_paths()[["ceda_data_folder"]],
                 machine_data_path = PFUSetup::get_abs_paths()[["machine_data_folder"]],
                 exemplar_table_path = PFUSetup::get_abs_paths()[["exemplar_table_path"]],
                 fu_analysis_folder = PFUSetup::get_abs_paths()[["fu_analysis_folder"]],
                 reports_source_folders = PFUSetup::get_abs_paths()[["reports_source_folders"]],
                 reports_dest_folder = PFUSetup::get_abs_paths()[["reports_dest_folder"]])


drake::make(plan)
