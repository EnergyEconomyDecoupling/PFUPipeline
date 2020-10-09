livestock_path <- paste(PFUSetup::get_abs_paths()$project_path, 
                        "/Data/FAO Data/FAOSTAT_data_10-9-2020.csv", sep = "")




































################################################################################
## FAOSTAT

# The package FAOSTATpackage (https://github.com/mkao006/FAOSTATpackage) is no 
# longer maintained as it's author no longer works for the FAO. 
# The FAO have updated their API's since which means we can only download the 
# bulk data through a development package on Gitlab also called FAOSTATpackage 
# (https://gitlab.com/paulrougieux/faostatpackage) which is maintained by a 
# different author


# Code to bulk download the livestock data
#### This is not as clean as simply downloading the required data from FAOSTAT directly

# install.packages("FAOSTAT")
# library(FAOSTAT)
# 
# data_folder <- "data_raw"
# dir.create(data_folder)
# 
# url_bulk_site <- "http://fenixservices.fao.org/faostat/static/bulkdownloads"
# 
# url_livestock <- "Production_Livestock_E_All_Data.zip"
# 
# url <- file.path(url_bulk_site, url_livestock)
# 
# download_faostat_bulk(url_bulk = url, data_folder = data_folder)
# 
# livestock <- read_faostat_bulk("data_raw/Production_Livestock_E_All_Data.zip")
