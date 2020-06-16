# Assign the file path to the variable 'filepath'

filepath <- "C:/Users/Zeke Marshall/Dropbox/Fellowship 1960-2015 PFU database/Country-level exergy accounting data"

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
  country <- readxl::read_excel(country_path)
    unique(country[,c('Ef.product','Destination', 'Machine', 'Eu.product')])
      na.omit(country)
        country[,c(2,1,3,4)]
  return(country)
}

test_world <- map_func(analysis_files[2])

lapply(analysis_files_list, map_func)

## NEED TO

# Add the origin of the mapping. oes it come from its own table, the exemplar table, or world table?
