library(tidyverse)

# Creates a file path to the FAO data in dropbox.
# This data contains the number of live animals, per species, per country, by year
animals_path <- paste(PFUSetup::get_abs_paths()$project_path, 
                        "/Data/FAO Data/FAOSTAT_data_10-9-2020.csv", sep = "")

# Reads the livestock data .csv file and creates a data frame
animals_data_raw <- readr::read_csv(animals_path) %>% 
  as.data.frame()

# Selects and renames the relevant columns
animals_data_trimmed <- animals_data_raw %>%
  dplyr::select(c("Area Code", "Area", "Item", "Year", "Value")) %>%
  magrittr::set_colnames(c("ISO_Country_Code", "Country", "Species", "Year", "Number"))

# Creates a list of livestock species
species <- unique(animals_data_trimmed$Species)

# Filters out "non-working" species
working_species_animals <- animals_data_trimmed %>%
  dplyr::filter(Species %in% c("Asses",
                               "Camels",
                               "Cattle",
                               "Horses",
                               "Mules",
                               "Buffaloes",
                               "Camelids, other"
                             )
                )

# Sets list of exemplar country codes
countries <- c("ESP", "PRT", "MEX", "GBR", "GHA", "CHN", "HND", "USA")

# Creates data frame of working animals for exemplar countries only
working_species_animals_exemplar <- working_species_animals %>%
  dplyr::filter(ISO_Country_Code %in% countries)

# Creates 
animals_plot <- ggplot2::ggplot(working_species_animals_exemplar) +
  ggplot2::geom_area(mapping = aes(x = Year, 
                                   y = Number,
                                   fill = Species)) +
  ggplot2::facet_wrap(vars(Country),
                      scales = "free_y")

# Creates a 2 column DF containing the country names and associated ISO codes in
# the FAO data
FAO_Country_ISO <- animals_data_trimmed %>%
  dplyr::select(c("ISO_Country_Code", "Country")) %>%
  unique()

# Writes a .csv file to the Mapping folder for use in Country_Mapping 
write.csv(FAO_Country_ISO, file = paste0(PFUSetup::get_abs_paths()$project_path, 
                                        "/Mapping/FAO_countries.csv", sep = ""))

# Creates a filepath to the country_mapping concordance file
country_mapping_path <- paste(PFUSetup::get_abs_paths()$project_path, 
                              "/Mapping/Country_Mapping_2019.xlsx", sep = "")

# Reads the exemplar_table sheet of the country mapping file, this contains 
# a list of corresponding continent/regions codes, ISO codes and IEA country names
country_mapping <- readxl::read_excel(country_mapping_path,
                                      sheet = "exemplar_table") %>%
  tibble::tibble()

# Selects relevant columns, and removes countries with no region code (i.e. World)
continent_concordance_iea <- country_mapping %>%
  dplyr::select(c("Region.code", "2017")) %>% # This will need to updated with each update of the IEA data!
  magrittr::set_colnames(c("Region.code", "ISO_Country_Code")) %>%
  dplyr::filter(Region.code != "")

# Creates a tibble from working_species_animals which only contains iea countries
# and adds a region.code column

pfu_working_species_animals <- working_species_animals %>%
  dplyr::right_join(continent_concordance_iea, by = "ISO_Country_Code")

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
