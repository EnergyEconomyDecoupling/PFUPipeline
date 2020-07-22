# This function collates all FU etas sheets from completed countries 
# and combines them into a data frame

# Creates a function which reads a FU Analysis file and extracts the etas time-series

etas_func <- function(country_path) {
  readxl::read_excel(country_path, sheet = "FU etas")
}


# Applies the etas_func function to each file within the list and then binds the data into a single data frame

etas_data <- lapply(analysis_files_list, etas_func) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(etas_data$Quantity == "eta.fu")

### Removes additional columns


# Generates a list of unique machines

machines <- unique(etas_data$Machine)
eu.products <- unique(etas_data$Eu.product)


# Creates DF's for each individual Machine
# EXAMPLE : Petrol cars

petrol_cars_MD <- filter(etas_data, etas_data$Machine == "Petrol cars")

ind_hf_100C <- filter(etas_data, etas_data$Machine == "Industrial heat/furnace")
ind_hf_100C <-  dplyr::filter(ind_hf_100C, ind_hf_100C$Eu.product == "MTH.100.C")
                         
                         
                        
                      
                      


