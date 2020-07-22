# Creates a function which reads FU Analysis file and creates a simplified mapping data frame

map_func <- function(country_path) {
  readxl::read_excel(country_path) %>%
    dplyr::select("Country", "Destination", "Ef.product", "Machine", "Eu.product") %>%
    unique() %>%
    na.omit()
}

# test_world <- map_func(analysis_files[2])

mapping_data <- lapply(analysis_files_list, map_func) %>% 
  dplyr::bind_rows()



## NEED TO

# Add the origin of the mapping. oes it come from its own table, the exemplar table, or world table?

# Add Ef.product groups
