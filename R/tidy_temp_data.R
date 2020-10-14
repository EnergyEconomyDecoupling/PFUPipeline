library(readr)
library(readxl)
library(dplyr)
library(magrittr)

## I need to create a function which loops through all CEDA FTP links,
# rather than calling individual functions for each metric

################################################################################

# Reads and combines mean temperature data into a single data frame

# Establishes destination directory for files
dest_file_tmp <- paste0(PFUSetup::get_abs_paths()$project_path, "/Data/Temperature Data/CEDA_2018/tmp", sep ="")

# Creates a list of CEDA tmp files in directory
tmp_files <- list.files(path = dest_file_tmp)

# Creates a list of directories for each file
tmp_dir <- paste(dest_file_tmp, "/", tmp_files, sep = "")

# Creates a function which reads individual tmp.per files
read_tmp <- function(file) {
  readr::read_table2(file, skip = 3) %>%
    as.data.frame() %>%
    dplyr::mutate(Country = substr(basename(file), 23, nchar(basename(file))-8)
                  )
}

# Calls read_tmp function on all files and binds them into a single data frame
all_tmp <- do.call(rbind, lapply(tmp_dir, read_tmp))  # How do i prevent the printouts in the console???

# Re-arranges data frame
all_tmp <- all_tmp %>%
  dplyr::relocate(Country, .before = YEAR) %>%
  tidyr::pivot_longer(!c("Country", "YEAR"), names_to = "Period", values_to = "tmp")

################################################################################

# Reads and combines max temperature data into a single data frame

# Establishes destination directory for files
dest_file_tmx <- paste0(PFUSetup::get_abs_paths()$project_path, "/Data/Temperature Data/CEDA_2018/tmx", sep ="")

# Creates a list of CEDA tmp files in directory
tmx_files <- list.files(path = dest_file_tmx)

# Creates a list of directories for each file
tmx_dir <- paste(dest_file_tmx, "/", tmx_files, sep = "")

# Creates a function which reads individual tmp.per files
read_tmx <- function(file) {
  readr::read_table2(file, skip = 3) %>%
    as.data.frame() %>%
    dplyr::mutate(Country = substr(basename(file), 23, nchar(basename(file))-8)
    )
}

# Calls read_tmp function on all files and binds them into a single data frame
all_tmx <- do.call(rbind, lapply(tmx_dir, read_tmx))

# Re-arranges data frame
all_tmx <- all_tmx %>%
  dplyr::relocate(Country, .before = YEAR) %>%
  tidyr::pivot_longer(!c("Country", "YEAR"), names_to = "Period", values_to = "tmx")

################################################################################

## Joins tmp and tmx data, and pivots longer again

all_data <- all_tmp %>%
  merge(all_tmx) %>%
  tidyr::pivot_longer(cols = c("tmp", "tmx"), names_to = "Metric", values_to = "Temperature")  

################################################################################

# Adds country codes assigned to IEA countries
mapping_table <- readxl::read_excel("C:/Users/Zeke Marshall/Dropbox/Fellowship 1960-2015 PFU database/Mapping/Country_Mapping_2020.xlsx",
                                     sheet = "exemplar_table")

# Reads mapping table and extracts CEDA country names included 
# in the PFU database, and associated country codes
CEDA_concordance <- mapping_table %>%
  dplyr::select("CEDA.name", "2018") %>%
  magrittr::set_colnames(c("Country", "ISO_Country_Code"))

# Adds ISO country codes to temperature data frame for PFU countries
pfu_data <- all_data %>%
  merge(CEDA_concordance, by = "Country") %>%
  dplyr::relocate(ISO_Country_Code, .before = Country)



################################################################################

## Extracts and exports a .csv of CEDA country names

# Creates a data frame with the CEDA Country names found in the file name
CEDA_countries <- unique(all_tmp$Country) %>%
  as.data.frame()

# Creates a .csv file with CEDA country names.
write.csv(CEDA_countries, 
          file = paste0(PFUSetup::get_abs_paths()$project_path, 
                        "/Database plan/", "CEDA_countries.csv", sep =""))


################################################################################



