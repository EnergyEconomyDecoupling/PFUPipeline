# The workflow plan data frame outlines the analyses to be conducted.

# Get a set of machine-specific paths
paths <- get_abs_paths()

# Create our plan
plan <- SEAPSUTWorkflow::get_plan(countries = "ESP", 
                                  max_year = 2017,
                                  iea_data_path = paths$iea_data_path, 
                                  exemplar_table_path = paths$exemplar_table_path, 
                                  fu_analysis_folder = paths$fu_analysis_folder)
