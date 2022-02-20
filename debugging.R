# library(PFUSetup)
# library(SEAPSUTWorkflow)
# 
# 
# max_year <- 2019                        
# countries <- c("WMB")
# additional_exemplars <- "WLD"
# 
# # Create our drake plan
# plan <- SEAPSUTWorkflow::get_plan(countries = countries, 
#                                   additional_exemplar_countries = additional_exemplars,
#                                   max_year = max_year,
#                                   iea_data_path = PFUSetup::get_abs_paths()[["iea_data_path"]],
#                                   country_concordance_path = PFUSetup::get_abs_paths()[["country_concordance_path"]],
#                                   phi_constants_path = PFUSetup::get_abs_paths()[["phi_constants_path"]],
#                                   ceda_data_folder = PFUSetup::get_abs_paths()[["ceda_data_folder"]],
#                                   machine_data_path = PFUSetup::get_abs_paths()[["machine_data_folder"]],
#                                   exemplar_table_path = PFUSetup::get_abs_paths()[["exemplar_table_path"]],
#                                   fu_analysis_folder = PFUSetup::get_abs_paths()[["fu_analysis_folder"]],
#                                   reports_source_folders = PFUSetup::get_abs_paths()[["reports_source_folders"]],
#                                   reports_dest_folder = PFUSetup::get_abs_paths()[["reports_dest_folder"]], 
#                                   release = FALSE)
# 
# drake::make(plan)