# Find the path to the GDP data
path <- PFUSetup::get_abs_paths()
project_path <- path$project_path
GDP_path <- paste(project_path,"/GDP Data/GDP_by_country.xlsx", sep = "")

# Set up the root directory
root <- rprojroot::is_rstudio_project

# Identifies the file path for the drake cache
cache_path <- root$find_file(".drake")

# Creates a data frame containing the GDP per capita constant 2010 US$ data
GDP_data_wide <- readxl::read_xlsx(GDP_path) %>%
  as.data.frame()

# Creates a data frame containing the fu efficiency data
etas <- drake::readd(SEAPSUTWorkflow::target_names$CompletedEfficiencyTables, 
                     path = cache_path, 
                     character_only = TRUE) %>%
  dplyr::filter(Quantity == "eta.fu")

etas$Machine_Eu.product = paste(etas$Machine," - ", etas$Eu.product)

# Creates a dataframe containing the countries list as a data frame
countries <- drake::readd(SEAPSUTWorkflow::target_names$countries, 
                          path = cache_path, 
                          character_only = TRUE)

#  Creates a list of years in the drake workflow
max_year <- drake::readd(SEAPSUTWorkflow::target_names$max_year, 
                         path = cache_path, 
                         character_only = TRUE)

years <- paste(1960:max_year)

# Removes unnecessary columns 
GDP_data_wide$`Country Name` <- NULL
GDP_data_wide$`Indicator Name` <- NULL
GDP_data_wide$`Indicator Code` <- NULL

# Transposes the GDP per capita data from wide to long format
GDP_data_long <- reshape2::melt(data = GDP_data_wide, id.vars = "Country Code", measure.vars = years) %>%
  magrittr::set_colnames(c("Country", "Year", "GDP_Per_Capita"))

# Sets Year column to numeric
GDP_data_long$Year <- as.numeric(as.character(GDP_data_long$Year))

# Filters GDP data for countries in workflow
GDP_data_final <- GDP_data_long %>%
  dplyr::filter(Country %in% countries)

# Merges data frames
etaGDP <- merge(etas, GDP_data_long, by = c("Year", "Country"), all.x = TRUE, sort = FALSE)



diesel_cars <- etaGDP %>%
  dplyr::filter(Machine == "Diesel cars") %>%
  as.data.frame()

diesel_cars_plot <- ggplot2::ggplot(data = diesel_cars, mapping = aes(x = GDP_Per_Capita, y = .values, color = Country)) + 
  ggplot2::geom_point() + 
  geom_smooth(method = "lm")

petrol_cars <- etaGDP %>%
  dplyr::filter(Machine == "Petrol cars") %>%
  as.data.frame()

petrol_cars_plot <- ggplot2::ggplot(data = petrol_cars, mapping = aes(x = GDP_Per_Capita, y = .values, color = Country)) + 
  ggplot2::geom_point() + 
  geom_smooth(method = "lm")
  
ind_stat <- etaGDP %>%
  dplyr::filter(Machine == "Industry static diesel engines") %>%
  as.data.frame()

ind_stat_plot <- ggplot2::ggplot(data = ind_stat, mapping = aes(x = GDP_Per_Capita, y = .values, color = Country)) + 
  ggplot2::geom_point() + 
  geom_smooth(method = "lm")

facet_grid()

cowplot::plot_grid(diesel_cars_plot, petrol_cars_plot, ind_stat_plot)

