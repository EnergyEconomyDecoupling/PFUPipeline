# Sets list of exemplar country codes
countries <- c("ESP", "PRT", "MEX", "GBR", "GHA", "CHN", "HND", "USA")

# Filters countries to only include exemplar countries
exemplar_data <- all_data %>%
  dplyr::filter(ISO_Country_Code %in% countries)

# Creates plot of summer (JJA) temperatures by exemplar country
summer_temp_plot <- exemplar_data %>%
  dplyr::filter(Period == "JJA") %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(mapping = aes(x = YEAR, 
                                   y = Temperature, 
                                   colour = ISO_Country_Code)) +
  ggplot2::geom_hline(yintercept = 15, linetype = 2, colour = "blue", size = 1) +
  annotate(geom = "text", x = 1925, 
           y = 19.5, label = "Current (CHN) & Proposed Air Con Temp", fontface="bold") +
  
  ggplot2::geom_hline(yintercept = 20, linetype = 2, colour = "blue", size = 1) +
  annotate(geom = "text", x = 1925, 
           y = 14.5, label = "Current (all other) Air Con Temp", fontface="bold") +
  
  ggplot2::geom_hline(yintercept = 25, linetype = 2, colour = "red", size = 1) +
  annotate(geom = "text", x = 1925, 
           y = 24.5, label = "Proposed Environmental Temp A (25C)", fontface="bold") +
  
  ggplot2::geom_hline(yintercept = 30, linetype = 2, colour = "red", size = 1) +
  annotate(geom = "text", x = 1925, 
           y = 29.5, label = "Proposed Environmental Temp B (30C)", fontface="bold") +
  
  ggplot2::ylim(0, NA) +
  ggplot2::scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  ggplot2::ylab("Mean Summer Temperature [C]") +
  ggplot2::xlab("Year") +
  MKHthemes::xy_theme() + 
  facet_wrap(vars(Metric))

directlabels::direct.label(summer_temp_plot, method = "last.qp")