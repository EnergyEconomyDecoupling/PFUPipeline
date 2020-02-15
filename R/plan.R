# The workflow plan data frame outlines the analyses to be conducted.

plan <- drake_plan(
  countries = c("ESP", "USA", "CAN"),
  paths = get_abs_paths(), 
  AllIEAData = load_tidy_iea_df(paths$iea_data_path),
  CountryIEAData = target(extract_country_data(AllIEAData, countries), dynamic = map(countries, .trace = countries)), 
  BalancedBefore = target(tidy_iea_df_balanced(calc_tidy_iea_df_balances(CountryIEAData)), dynamic = map(countries, .trace = countries)), 
  Balanced = target(fix_tidy_iea_df_balances(CountryIEAData), dynamic = map(countries, .trace = countries)), 
  BalancedAfter = target(tidy_iea_df_balanced(calc_tidy_iea_df_balances(Balanced)), dynamic = map(countries, .trace = countries)), 
  Specified = target(specify_all(Balanced), dynamic = map(countries, .trace = countries)), 
  PSUT = target(prep_psut(Specified), dynamic = map(countries, .trace = countries)) 
)
