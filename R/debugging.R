# Matt found this code helpful when debugging functions that sit in other packages.
# Simply copy this code into an untitled file inside RStudio for the package you want to debug. 
# Then set breakpoints where necessary.
# When done, don't forget to delete the .drake directory.


# library(SEAPSUTWorkflow)
# 
# plan <- get_plan(countries = "ESP", max_year = 1960,
#                  iea_data_path = "~/Dropbox/Fellowship 1960-2015 PFU database/IEA extended energy balance data/IEA 2019 energy balance data/IEA Extended Energy Balances 2019.csv",
#                  exemplar_table_path = "~/Dropbox/Fellowship 1960-2015 PFU database/Database plan/Exemplar_Table.xlsx",
#                  fu_analysis_folder = "~/Dropbox/Fellowship 1960-2015 PFU database/Country-level exergy accounting data")
# 
# 
# drake::make(plan)
