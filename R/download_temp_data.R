library(RCurl)

################################################################################

## Mean monthly temperature

# Establishes FTP url for CEDA mean temperature (tmp)
url_tmp <- "ftp://ftp.ceda.ac.uk/badc/cru/data/cru_cy/cru_cy_4.03/data/tmp/"

# Establishes destination directory for files
dest_file_tmp <- paste0(PFUSetup::get_abs_paths()$project_path, "/Data/Temperature Data/CEDA_2018/tmp", sep ="")

# protocol <- "sftp"

# Sets username and password
up <- "zmarshall:ParrotIron4"

# Parses file names
filenames_tmp <- RCurl::getURL(url_tmp, userpwd = up,
                           ftp.use.epsv = TRUE, dirlistonly = TRUE)

# Separates file names and pastes the FTP URL to create a list of individual FTP links
filenames_tmp <- paste(url_tmp, strsplit(filenames_tmp, "\r*\n")[[1]], sep = "")

# Generates curl handle
con <- getCurlHandle(ftp.use.epsv = TRUE, userpwd = up)

# Downloads all countries into dest_file
for(file in filenames_tmp) {
  writeBin(getBinaryURL(file, curl = con, dirlistonly = FALSE),
           paste(dest_file_tmp, substr(file, nchar(url_tmp), nchar(file)), sep = ""))
}

################################################################################

## Maximum daily temperature, mean by month

# Establishes FTP url for CEDA max temperature (tmx)
url_tmx <- "ftp://ftp.ceda.ac.uk/badc/cru/data/cru_cy/cru_cy_4.03/data/tmx/"

# Establishes destination directory for files
dest_file_tmx <- paste0(PFUSetup::get_abs_paths()$project_path, "/Data/Temperature Data/CEDA_2018/tmx", sep ="")

# protocol <- "sftp"

# Sets username and password
up <- "zmarshall:ParrotIron4"

# Parses file names
filenames_tmx <- RCurl::getURL(url_tmx, userpwd = up,
                           ftp.use.epsv = TRUE, dirlistonly = TRUE)

# Separates file names and pastes the FTP URL to create a list of individual FTP links
filenames_tmx <- paste(url_tmx, strsplit(filenames_tmx, "\r*\n")[[1]], sep = "")

# Generates curl handle
con <- getCurlHandle(ftp.use.epsv = TRUE, userpwd = up)

# Downloads all countries into dest_file
for(file in filenames_tmx) {
  writeBin(getBinaryURL(file, curl = con, dirlistonly = FALSE),
           paste(dest_file_tmx, substr(file, nchar(url_tmx), nchar(file)), sep = ""))
}
