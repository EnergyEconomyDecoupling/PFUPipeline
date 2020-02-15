# The workflow plan data frame outlines the analyses to be conducted.

plan <- drake_plan(
  countries = c("ESP", "USA", "CAN"),
  paths = get_abs_paths(), 
  # Grab all the IEA data for ALL countries
  AllIEAData = load_tidy_iea_df(paths$iea_data_path),
  # Narrow down to only the countries of interest to us
  # and group in preparation for fixing energy balances
  CountryIEAData = target(extract_country_data(AllIEAData, countries) %>% 
                            group_by(Country, Method, Energy.type, Last.stage, Year, Product),
                          dynamic = map(countries, .trace = countries)), 
  # Check whether energy products are balanced.
  # They're not. Use readd(BalancedBefore) to see the results.  
  # FALSE indicates a country with at least one balance problem.
  BalancedBefore = target(CountryIEAData %>% calc_tidy_iea_df_balances() %>% tidy_iea_df_balanced(), 
    dynamic = map(countries, .trace = countries)), 
  # Balance all of the data by product and year.
  Balanced = target(fix_tidy_iea_df_balances(CountryIEAData), dynamic = map(countries, .trace = countries)), 
  # Check that everything is balanced.
  # Use readd(BalancedAfter) to show that flows are balanced for each country.
  # TRUE indicates balance has been achieved.
  BalancedAfter = target(Balanced %>% calc_tidy_iea_df_balances() %>% tidy_iea_df_balanced(), 
                          dynamic = map(countries, .trace = countries)), 
  # Specify the Balanced data frame by being more careful with names, etc.
  Specified = target(specify_all(Balanced), dynamic = map(countries, .trace = countries)), 
  # Arrange all the data into PSUT matrices.
  PSUT = target(prep_psut(Specified), dynamic = map(countries, .trace = countries)) 
)
