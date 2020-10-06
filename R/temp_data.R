library(RCurl)
library(readr)
library(readxl)
library(dplyr)
library(magrittr)
################################################################################

# Downloads temperature data fr each country from CEDA via FTP

# Establishes url for CEDA FTP
url <- "ftp://ftp.ceda.ac.uk/badc/cru/data/cru_cy/cru_cy_4.03/data/tmp/"

# Establishes destination directory for files
dest_file <- paste0(PFUSetup::get_abs_paths()$project_path, "/Data/Temperature Data/CEDA_2018", sep ="")

# protocol <- "sftp"

# Sets username and password
up <- "zmarshall:ParrotIron4"

# Parses file names
filenames <- RCurl::getURL(url, userpwd = up,
                    ftp.use.epsv = TRUE, dirlistonly = TRUE)

# Separates file names and pastes the FTP URL to create a list of individual FTP links
filenames <- paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")

# Generates curl handle
con <- getCurlHandle(ftp.use.epsv = TRUE, userpwd = up)

# Downloads all countries into dest_file
for(file in filenames) {
  writeBin(getBinaryURL(file, curl = con, dirlistonly = FALSE),
                            paste(dest_file, substr(file, nchar(url), nchar(file)), sep = ""))
}

################################################################################

# Reads and combines temperature data into a single data frame

# Creates a list of CEDA tmp files in directory
tmp_files <- list.files(path = dest_file)

# Creates a list of directories for each file
tmp_dir <- paste(dest_file, "/", tmp_files, sep = "")

# # Creates an empty data frame to hold all temperature data
# all_tmp <- tibble::tibble()

# Creates a function which reads individual tmp.per files
read_tmp <- function(file) {
  readr::read_table2(file, skip = 3) %>%
    as.data.frame() %>%
    dplyr::mutate(Country = substr(basename(file), 23, nchar(basename(file))-8)
                  )
}

# Calls read_tmp function on all files and binds them into a single data frame
all_tmp <- do.call(rbind, lapply(tmp_dir, read_tmp)
                   )

# Creates a data frame with the CEDA Country names found in the file name
CEDA_countries <- unique(all_tmp$Country) %>%
  as.data.frame()

# Creates a .csv file with CEDA country names.
#write.csv(CEDA_countries, file = paste0(PFUSetup::get_abs_paths()$project_path, "/Database plan/", "CEDA_countries.csv", sep =""))

# Adds country codes assigned to IEA countries
Exemplar_Table <- readxl::read_excel("C:/Users/Zeke Marshall/Dropbox/Fellowship 1960-2015 PFU database/Database plan/Exemplar_Table.xlsx",
                                     sheet = "exemplar_table")

CEDA_concordance <- Exemplar_Table %>%
  dplyr::select("CEDA.name", "2017") %>%
  magrittr::set_colnames(c("Country", "Country_code"))

all_tmp <- all_tmp %>%
  merge(CEDA_concordance, by = "Country")
