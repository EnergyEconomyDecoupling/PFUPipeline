# Assign the file path to the variable 'filepath'

filepath <- "C:/Users/Zeke Marshall/Dropbox/Fellowship 1960-2015 PFU database/Country-level exergy accounting data"

# List the file path for each countries folder

countries <- list.files(path = filepath, full.names = TRUE)

# List the file path for each "FU Analysis" file

analysis_files <- list.files(path = filepath, recursive = TRUE, full.names = TRUE, pattern = "* FU Analysis.xlsx")

analysis_files <- data.frame(analysis_files)


# Attempted methods to exclude "~$" files 

analysis_files <- analysis_files[ !grepl("~$", analysis_files) ]

analysis_files <- dplyr::filter(analysis_files, !grepl('~$', analysis_files))

analysis_files <- subset(analysis_files, grepl("*~$", analysis_files))


# Loop through each folder and extract mapping information

for country in analysis_files {
  readxl::read_excel(country) %>%
    unique(country[,c('Ef.product','Destination', 'Machine', 'Eu.product')]) %>%
      na.omit() %>%
        country[,c(2,1,3,4)]
  
  # Add function to generate mapping table here 
}



# list_analysis_files <- function(countries) { list.files(full.names = TRUE, pattern = '*Analysis.xlsx') }

# analysis_files2 <- do.call(rbind, (countries, list_analysis_files))

# do.call(rbind, lapply(countries, list_analysis_files))