
# Set up the root directory
root <- rprojroot::is_rstudio_project

# Identifies the file path for the drake cache
cache_path <- root$find_file(".drake")

tn <- SEAPSUTWorkflow::target_names %>% View


allocation_graphs <- drake::readd(SEAPSUTWorkflow::target_names$AllocationGraphs, 
                            path = cache_path, 
                            character_only = TRUE)
res_elec_al_gr <- allocation_graphs %>%
  dplyr::filter(Ef.product == "Electricity", Destination == "Residential")  # or input$Ef.product and input$Destination

World <- res_elec_al_gr$plots[[1]]

World2 <- res_elec_al_gr %>%
  dplyr::filter(Country == "World") # or input$Country

World3 <- World2$plots[[1]]

ESP <- res_elec_al_gr$plots[[2]]
  
GRC <- res_elec_al_gr$plots[[3]]

cowplot::plot_grid(World, ESP, GRC, ncol=2, labels = "AUTO" )
