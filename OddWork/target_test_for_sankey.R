# Creates a list of target names
target_name_list <- SEAPSUTWorkflow::target_names

# Creates various data frames containing data for the world
World <- SEAPSUTWorkflow::readd_by_country("PSUT_final", "World") %>%
  dplyr::mutate(
    U = matsbyname::sum_byname(U_EIOU, U_feed)) %>%
  dplyr::relocate(U, .after = U_feed)

# Creates a list of column names from the World PSUT_final target
PSUT_final_World_colnames <- as.data.frame(colnames(World))

World_U <- World %>% 
  dplyr::select(Year, U)

World_U_1971 <- World_U %>%
  dplyr::filter(Year == "1971") %>%
  dplyr::select(U)

World_1971 <- World %>%
  dplyr::filter(Year == "1971")

# World_long <- World %>%
#   tidyr::pivot_longer(-Year, names_to = "matrix.name", values_to = "matrix")

# Set the metadata columns
World_meta_columns <- c(IEATools::iea_cols$country,
                        IEATools::iea_cols$year,
                        IEATools::iea_cols$method,
                        IEATools::iea_cols$energy_type,
                        IEATools::iea_cols$last_stage)

# Pivot on the difference between the columns present and the metadata columns
World_long <- World %>% 
  tidyr::pivot_longer(cols = all_of(setdiff(PSUT_final_World_colnames, World_meta_columns)), 
                      names_to = "matnames", values_to = "matvals")

# Test make-sankey using example method. 
World_long %>%
  tidyr::spread(key = "matnames", value = "matvals") %>% # This converts World_long to World (or World_tidy)
  Recca::make_sankey(fontSize = 15, height = 1600) %>%
  magrittr::extract2("Sankey") %>%
  magrittr::extract2(1)

# Code to make a sankey diagram for a specific year from the original target (World or World_tidy)
World_sankey %>%
  dplyr::filter(Year == "2017") %>%
  Recca::make_sankey(fontSize = 15, height = 1300, width = 1800) %>%
  magrittr::extract2("Sankey") %>%
  magrittr::extract2(1)

################################################################################

# Creates various data frames containing data for ESP
ESP_tidy <- SEAPSUTWorkflow::readd_by_country("PSUT_final", "ESP") %>%
  dplyr::mutate(
    U = matsbyname::sum_byname(U_EIOU, U_feed)) %>%
  dplyr::relocate(U, .after = U_feed)

ESP_tidy %>%
  dplyr::filter(Year == "2017") %>%
  Recca::make_sankey(fontSize = 15, height = 1000, width = 1800) %>%
  magrittr::extract2("Sankey") %>%
  magrittr::extract2(1)

################################################################################


# Working sankey code for the example dataset
UKEnergy2000matsData <- Recca::UKEnergy2000mats
#View(UKEnergy2000matsData)
UKEnergy2000matsData %>%
  tidyr::spread(key = "matrix.name", value = "matrix") %>%
  Recca::make_sankey(fontSize = 15, height = 400, width = 800) %>%
  magrittr::extract2("Sankey") %>%
  magrittr::extract2(1)
