# Loads required packages
library(wbstats)
library(FAOSTAT)
library(Rilostat)

# Sets list of exemplar country codes

countries <- c("ESP", "PRT", "MEX", "GBR", "GHA", "CHN", "HND", "USA")


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
  merge(ind_workers_per) %>% # MELT HERE
  magrittr::set_colnames(c("ISO_Country_Code", "Country", "Year", "Total_Population", "WorkingAge_Population_Per", "Participation_Rate_Per", "Agriculture_per", "Services_per", "Industry_per"))

# Calculates working population in a new column
human_labor_data <- human_labor_data %>%
  dplyr::mutate("Working_Population" = Total_Population * (WorkingAge_Population_Per/100) * (Participation_Rate_Per/100))

# Calculates the total number of workers in agriculture, services and industry in new columns
human_labor_data <- human_labor_data %>% # GROUP BY HERE
  dplyr::mutate("Agriculture_Workers" = Working_Population * (Agriculture_per/100)) %>%
  dplyr::mutate("Services_Workers" = Working_Population * (Services_per/100)) %>%
  dplyr::mutate("Industry_Workers" = Working_Population * (Industry_per/100))


# Creates a ddf containing unique countries in the wbstats data
uniq_wb_countries_ISO <- unique(human_labor_data$ISO_Country_Code) %>%
  as.data.frame()


# Re-organises data frame, should I do this before I calculate the workers?
human_labor_data_long <- human_labor_data %>%
  # reshape2::melt(measure.vars = c("Agriculture_Workers", 
  #                                 "Services_Workers", 
  #                                 "Industry_Workers"),
  #                value.name = "Number",
  #                variable.name = "SectorNumber"
  #                ) %>%
  reshape2::melt(measure.vars = c("Agriculture_per",
                                  "Services_per",
                                  "Industry_per"),
                 value.name = "Percentage",
                 variable.name = "SectorShare"
                 )

human_labor_data_long_exemplars <- human_labor_data_long %>%
  dplyr::filter(ISO_Country_Code %in% countries)

shares_plot <- ggplot2::ggplot(human_labor_data_long_exemplars) +
  ggplot2::geom_area(mapping = aes(x = Year, 
                                   y = Percentage,
                                   # group = SectorShare,
                                   fill = SectorShare)
  ) +
  ggplot2::facet_wrap(vars(Country))

################################################################################

df1 <- reshape2::melt(human_labor_data, measure.vars = c("Agriculture_Workers", 
                                                       "Services_Workers", 
                                                       "Industry_Workers"),
                      value.name = "Number",
                      variable.name = "Sector_num")
df1 <- df1[,-c(7:9)]

df2 <- reshape2::melt(human_labor_data, measure.vars = c("Agriculture_per",
                                                       "Services_per",
                                                       "Industry_per"),
                      value.name = "Percentage",
                      variable.name = "Sector_per")
df2 <- df2[,-c(8:10)]

df3 <- df1 %>%
  cbind(df2[c("Sector_per", "Percentage")]) %>%
  dplyr::filter(ISO_Country_Code %in% countries)

numbers_plot <- ggplot2::ggplot(df3) +
  ggplot2::geom_area(mapping = aes(x = Year, 
                                   y = Number,
                                   # group = SectorShare,
                                   fill = Sector_per)
  ) +
  ggplot2::facet_wrap(vars(Country),
                      scales = "free_y")

# I need to reshape this ata earlier in the workflow to prevent having to create 
# two df's to align perc and num workers by sector



################################################################################
## FAOSTAT

# The package FAOSTATpackage (https://github.com/mkao006/FAOSTATpackage) is no 
# longer maintained as it's author no longer works for the FAO. 
# The FAO have updated their API's since which means we can only download the 
# bulk data through a development package on Gitlab also called FAOSTATpackage 
# (https://gitlab.com/paulrougieux/faostatpackage) which is maintained by a 
# different author

# Searches for the domain,  element and item code for a specific FAOSTAT query
FAOSTAT::FAOsearch



################################################################################
## ILOSTAT