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


# Add Ef.product groups



# Creates a function which reads FU Analysis file and creates a simplified mapping data frame
# Do I create a for loop inside this function?
map_func <- function(country_path) {
  readxl::read_excel(country_path) %>%
    dplyr::select("Destination", "Ef.product", "Machine", "Eu.product") %>%
    unique() %>%
    na.omit()
}

test_world <- map_func(analysis_files[2])

lapply(analysis_files_list, map_func)

## NEED TO

# Add the origin of the mapping. oes it come from its own table, the exemplar table, or world table?
