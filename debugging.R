
eta_fu_df <- readr::read_rds("/home/eeear/Downloads/eta_fu_Y_eiou.rds")

a <- eta_fu_df |> 
  filter(Country == "WRLD") |> 
  select(Country, Year, eta_fu_Y_E, eta_fu_EIOU_E) |> 
  pivot_longer(cols = starts_with("eta"), names_to = "matnames", values_to = "matvals") |> 
  matsindf::expand_to_tidy()

a |> filter(str_detect(colnames, "Non-energy")) |> 
  filter(matvals > 0) |> 
  print()



a |> 
  distinct(colnames) |>
  View()



