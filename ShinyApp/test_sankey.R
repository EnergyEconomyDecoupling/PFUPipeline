# Set up the root directory
root <- rprojroot::is_rstudio_project

# Identifies the file path for the drake cache
cache_path <- root$find_file(".drake")

selected_data_sankey_test <- drake::readd(SEAPSUTWorkflow::target_names$PSUT_final, 
             path = cache_path, 
             character_only = TRUE) %>%
  dplyr::mutate(U = matsbyname::sum_byname(U_EIOU, U_feed)) %>% # Here I create U manually, this needs to be done in the drake workflow
  
  dplyr::relocate(U, .after = U_feed) %>%
  
  dplyr::filter(Country == "ESP", Year == "1980")


selected_data_sankey_test %>%
  Recca::make_sankey(fontSize = 15, 
                     width = "1800", # both width = and height = do not work when passed as arguments into make_sankey
                     height = "1000",
                     nodeWidth = 30) %>%
  magrittr::extract2("Sankey") %>%
  magrittr::extract2(1)
