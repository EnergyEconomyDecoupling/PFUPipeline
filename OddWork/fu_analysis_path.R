# Assign the file path to the variable 'filepath'

filepath <- get_abs_paths()$fu_analysis_path

# List the file path for each countries folder

countries <- list.files(path = filepath, full.names = TRUE)

# List the file path for each "FU Analysis" file

analysis_files <- list.files(path = filepath, recursive = TRUE, full.names = TRUE, pattern = "* FU Analysis.xlsx")

analysis_files <- data.frame(analysis_files)

colnames(analysis_files) <- c("country_path")

analysis_files <- analysis_files[!grepl(as.character("~$"), analysis_files$country_path, fixed = TRUE),]

analysis_files_list <- as.list(analysis_files)
