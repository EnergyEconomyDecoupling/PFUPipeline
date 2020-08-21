# Creates a list of target names
target_name_list <- SEAPSUTWorkflow::target_names

# Creates a lis of column names from the World PSUT_final target
PSUT_final_World_colnames <- colnames(World)

# Creates various data frames containing data for the world
World <- SEAPSUTWorkflow::readd_by_country("PSUT_final", "World") %>%
  dplyr::mutate(
    U = matsbyname::sum_byname(U_EIOU, U_feed)) %>%
  dplyr::relocate(U, .after = U_feed)

World_U <- World %>% 
  dplyr::select(Year, U)

World_U_1971 <- World_U %>%
  dplyr::filter(Year == "1971") %>%
  dplyr::select(U)

World_1971 <- World %>%
  dplyr::filter(Year == "1971")

World_long <- World %>%
  tidyr::pivot_longer(-Year, names_to = "matrix.name", values_to = "matrix")

# Test make-sankey using example method..
World_U_1971 %>%
  tidyr::spread(key = "U", value = "U") %>%
  Recca::make_sankey(fontSize = 15, height = 1600) %>%
  magrittr::extract2("Sankey") %>%
  magrittr::extract2(1)

# Working sankey code for the example dataset
UKEnergy2000matsData <- Recca::UKEnergy2000mats

View(UKEnergy2000matsData)

UKEnergy2000matsData %>%
  tidyr::spread(key = "matrix.name", value = "matrix") %>%
  Recca::make_sankey(fontSize = 15, height = 400, width = 800) %>%
  magrittr::extract2("Sankey") %>%
  magrittr::extract2(1)


# Attempt to create sankey diagram using default usage as in function description
World_1971_sankey <- make_sankey(.sutmats = World_1971,
                                  R = "R",
                                  U = "U",
                                  V = "V",
                                  Y = "Y",
                                  simplify_edges = TRUE,
                                  sankey = "Sankey")