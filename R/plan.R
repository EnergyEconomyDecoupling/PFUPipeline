# The workflow plan data frame outlines the analyses to be conducted.

plan <- drake_plan(
  # # Try static branching
  # max_year = 2017,
  # paths = get_abs_paths(),
  # # Grab all the IEA data for ALL countries
  # AllIEAData = load_tidy_iea_df(paths$iea_data_path),
  # # Narrow down to only the countries of interest to us
  # # and group in preparation for fixing energy balances
  # IEAData = target(extract_country_data(AllIEAData, countries, max_year), transform = map(countries = c("ESP", "USA"))),
  # # Balance all of the data by product and year.
  # BalancedIEAData = target(fix_tidy_iea_df_balances(IEAData), transform = map(countries)),
  # # Specify the BalancedIEAData data frame by being more careful with names, etc.
  # Specified = target(specify_all(BalancedIEAData), transform = map(countries)),
  # # Arrange all the data into PSUT matrices.
  # PSUT = target(prep_psut(Specified), transform = map(countries))
  
  
  # Trying dynamic branching
  
  countries = c("ESP", "USA"),
  max_year = 2017,
  paths = get_abs_paths(),
  # Grab all the IEA data for ALL countries
  AllIEAData = load_tidy_iea_df(paths$iea_data_path),
  # Narrow down to only the countries of interest to us
  # and group in preparation for fixing energy balances
  IEADataSlimmed = filter(AllIEAData, Country %in% countries, Year <= max_year),
  country_var_in_IEADataSlimmed = IEADataSlimmed$Country,
  IEAData = target(IEADataSlimmed,
                   dynamic = group(IEADataSlimmed, .by = country_var_in_IEADataSlimmed)),
  # Check whether energy products are balanced.
  # They're not. Use readd(BalancedBefore) to see the results.
  # FALSE indicates a country with at least one balance problem.
  # BalancedBefore = target(IEAData %>% calc_tidy_iea_df_balances() %>% tidy_iea_df_balanced()),
  # Balance all of the data by product and year.
  # BalancedIEAData = target(fix_tidy_iea_df_balances(IEAData), dynamic = map(countries, .trace = countries)),
  # BalancedIEAData = target(balance(IEAData, countries), dynamic = map(countries, .trace = countries)),
  
  
  
  
  
  # # Check that everything is balanced.
  # # Use readd(BalancedAfter) to show that flows are balanced for each country.
  # # TRUE indicates balance has been achieved.
  # BalancedAfter = target(BalancedIEAData %>% calc_tidy_iea_df_balances() %>% tidy_iea_df_balanced(),
  #                        dynamic = map(countries, .trace = countries)),
  # OKToProceed = stopifnot(all(BalancedAfter)),
  # # Specify the BalancedIEAData data frame by being more careful with names, etc.
  # Specified = target(specify_all(BalancedIEAData), dynamic = map(countries, .trace = countries)),
  # # Arrange all the data into PSUT matrices.
  # PSUT = target(prep_psut(Specified), dynamic = map(countries, .trace = countries))
)
