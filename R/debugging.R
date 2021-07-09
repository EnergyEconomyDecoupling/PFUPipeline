# Matt found this code helpful when debugging functions that sit in other packages.
# Simply copy this code into an untitled file inside RStudio for the package you want to debug. 
# Then set breakpoints where necessary.
# When done, don't forget to delete the .drake directory.


# library(SEAPSUTWorkflow)
# 
# plan <- get_plan(countries = c("HND"),
#                  max_year = 1971,
#                  iea_data_path = "~/Dropbox/Fellowship 1960-2015 PFU database/IEA extended energy balance data/IEA 2019 energy balance data/IEA Extended Energy Balances 2019.csv",
#                  ceda_data_folder = file.path(PFUSetup::get_abs_paths()[["project_path"]], "Data", "CEDA Data"),
#                  machine_data_path = file.path(PFUSetup::get_abs_paths()[["project_path"]], "Data", "Machines - Data"),
#                  exemplar_table_path = "~/Dropbox/Fellowship 1960-2015 PFU database/Database plan/Exemplar_Table.xlsx",
#                  fu_analysis_folder = "~/Dropbox/Fellowship 1960-2015 PFU database/Country-level exergy accounting data",
#                  reports_source_folders = PFUSetup::get_abs_paths()[["reports_source_folders"]],
#                  reports_dest_folder = PFUSetup::get_abs_paths()[["reports_dest_folder"]])
# 
# 
# drake::make(plan)
