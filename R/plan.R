# The workflow plan data frame outlines the analyses to be conducted.


paths <- get_abs_paths()

# Create our plan
plan <- SEAPSUTWorkflow::get_plan(countries = countries, 
                                  max_year = max_year,
                                  iea_data_path = paths$iea_data_path, 
                                  exemplar_table_path = paths$exemplar_table_path, 
                                  fu_analysis_folder = paths$fu_analysis_folder)
