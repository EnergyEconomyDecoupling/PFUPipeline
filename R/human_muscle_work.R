# Loads required packages
library(wbstats)
library(tidyverse)
#library(Rilostat)


################################################################################
## WORLD BANK

# Interestingly trying to download data with wbstats::wb_data() fails with the 
# error code: Error: World Bank API request failed for indicator SL.TLF.ACTI.ZS
#             message: Server error: (502) Bad Gateway
#             category: Server error
#             reason: Bad Gateway 
# but using wb_data() succeeds.

# Provides a snapshot of available countries, indicators, and other relevant information
str(wb_cachelist, max.level = 1)

# Updates World Bank API cache, stores in list
new_cache <- wb_cache()

# Searches for indicators containing relevant terms
population_inds <- wb_search("total population")
services_inds <- wb_search("services")
agriculture_inds <- wb_search("agriculture")
industry_inds <- wb_search("industry")

# Creates a dataframe containing total population by country
population <- wb_data("SP.POP.TOTL") %>%
  dplyr::select(2:5)

# Downloads data for Population ages 15-64 (% of total population)
population_15_64 <- wb_data("SP.POP.1564.TO.ZS") %>%
  dplyr::select(2:5)

# Downloads data for "Labor force participation rate, total (% of total population ages 15-64) (modeled ILO estimate)"
participation_rate <- wb_data("SL.TLF.ACTI.ZS") %>%
  dplyr::select(2:5)

# Downloads data for "Employment in agriculture (% of total employment) (modeled ILO estimate)"
agr_workers_per <- wb_data("SL.AGR.EMPL.ZS") %>%
  dplyr::select(2:5)

# Downloads data for "Employment in services (% of total employment) (modeled ILO estimate)"
srv_workers_per <- wb_data("SL.SRV.EMPL.ZS") %>%
  dplyr::select(2:5)

# Downloads data for "Employment in industry (% of total employment) (modeled ILO estimate)"
ind_workers_per <- wb_data("SL.IND.EMPL.ZS") %>%
  dplyr::select(2:5)


# Combines data into a single data frame
human_labor_data <- population %>%
  merge(population_15_64) %>%
  merge(participation_rate) %>%
  merge(agr_workers_per) %>%
  merge(srv_workers_per) %>%
  merge(ind_workers_per) %>%
  magrittr::set_colnames(c("ISO_Country_Code", 
                           "Country", 
                           "Year", 
                           "Total_Population", 
                           "WorkingAge_Population_Per", 
                           "Participation_Rate_Per", 
                           "Agriculture", "Services", "Industry"))

# Calculates working population in a new column
human_labor_data <- human_labor_data %>%
  dplyr::mutate("Working_Population" = Total_Population * (WorkingAge_Population_Per/100) * (Participation_Rate_Per/100))

# Reshapes data for the percentage of workers in each sector from wide to long format
human_labor_data <- human_labor_data %>%
  reshape2::melt(measure.vars = c("Agriculture", "Services", "Industry"),
                 value.name = "Percentage_Workers_Sector",
                 variable.name = "Sector")
  
# Calculates the workers per sector
human_labor_data <- human_labor_data %>%
  dplyr::mutate("Workers_Sector" = Working_Population * Percentage_Workers_Sector)

# Sets list of exemplar country codes
countries <- c("ESP", "PRT", "MEX", "GBR", "GHA", "CHN", "HND", "USA")

# Filters data frame to only include exemplar countries
human_labor_data_exemplars <- human_labor_data %>%
  dplyr::filter(ISO_Country_Code %in% countries)

# # Filters data to only include data for the USA as an example
# USA <- human_labor_data_exemplars %>%
#   dplyr::filter(ISO_Country_Code == "USA")

# Creates a grid of plots containing percentage shares in each sector by country
shares_plot <- ggplot2::ggplot(human_labor_data_exemplars) +
  ggplot2::geom_area(mapping = aes(x = Year, 
                                   y = Percentage_Workers_Sector,
                                   fill = Sector)) +
  ggplot2::facet_wrap(vars(Country))


# Creates a grid of plots containing worker numbers in each sector by country
numbers_plot <- ggplot2::ggplot(human_labor_data_exemplars) +
  ggplot2::geom_area(mapping = aes(x = Year, 
                                   y = Workers_Sector,
                                   fill = Sector)) +
  ggplot2::facet_wrap(vars(Country),
                      scales = "free_y")

# Creates a 2 column DF containing the country names and associated ISO codes in
# the World Bank data
WB_Country_ISO <- human_labor_data %>%
  dplyr::select(c("ISO_Country_Code", "Country")) %>%
  unique()

# Writes a .csv file to the Mapping folder for use in Country_Mapping 
write.csv(WB_Country_ISO, file = paste0(PFUSetup::get_abs_paths()$project_path, 
                                        "/Mapping/WB_countries.csv", sep = ""))


# Creates a filepath to the country_mapping concordance file
country_mapping_path <- paste(PFUSetup::get_abs_paths()$project_path, 
                              "/Mapping/Country_Mapping.xlsx", sep = "")

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

# Creates a tibble from human_labor_data which only contains iea countries
# and adds a region.code column

pfu_human_labor_data <- human_labor_data %>%
  dplyr::right_join(continent_concordance_iea, by = "ISO_Country_Code")

################################################################################
## ILOSTAT