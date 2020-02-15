# The workflow plan data frame outlines the analyses to be conducted.

plan <- drake_plan(
  countries = c("ESP", "USA", "CAN"),
  paths = get_abs_paths(), 
  AllIEAData = load_tidy_iea_df(paths$iea_data_path),
  CountryIEAData = target(extract_country_data(AllIEAData, countries), dynamic = map(countries, .trace = countries)), 
  Balances = target(calc_tidy_iea_df_balances(CountryIEAData), dynamic = map(countries, .trace = countries)), 
  Balanced = target(tidy_iea_df_balanced(CountryIEAData), dynamic = map(countries, .trace = countries)), 
  SpecifiedCountryIEAData = target(specify_all(CountryIEAData), dynamic = map(countries, .trace = countries)), 
  PreppedSpecifiedCountryIEAData = target(prep_psut(SpecifiedCountryIEAData), dynamic = map(countries, .trace = countries)) 
)
