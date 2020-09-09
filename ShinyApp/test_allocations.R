# Set up the root directory
root <- rprojroot::is_rstudio_project

# Identifies the file path for the drake cache
cache_path <- root$find_file(".drake")


allocations <- drake::readd(SEAPSUTWorkflow::target_names$CompletedAllocationTables, 
                            path = cache_path, 
                            character_only = TRUE)

# Adds a combined Machine-Eu.product column
allocations$Machine_Eu.product = paste(allocations$Machine," - ", allocations$Eu.product)


selected_data_allocations_test <- allocations %>%
  dplyr::filter(Ef.product == "Electricity",
                Destination == "Residential")



ggplot2::ggplot(data = selected_data_allocations_test) +
  geom_area(mapping = aes(x = Year, y = .values, 
                          group = Machine_Eu.product, 
                          #colour = Machine_Eu.product,  
                          fill = Machine_Eu.product 
  ),
  position = "fill" # "fill" does not cause gaps in the data for individual countries, but it grossly deforms the data for multiple plots
  #position = "stack" # "stack" causes gaps in the data for single countries, and the data to overlay on multiple plots rather than stack, but the proportions remain the same
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  MKHthemes::xy_theme() + 
  facet_wrap(vars(Country),
             #ncol = 1, # Arranging by rows or columns makes no difference to the stacking issue.
             #nrow = 1,
             scales = "free_x"
  )
