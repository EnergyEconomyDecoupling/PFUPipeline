# The workflow plan data frame outlines the analyses to be conducted.

plan <- drake_plan(

  countries = c("ESP", "USA"),
  max_year = 2017,
  paths = get_abs_paths(),
  
  # Grab all the IEA data for ALL countries
  AllIEAData = load_tidy_iea_df(paths$iea_data_path),
  
  # Narrow down to only the countries of interest to us
  # and group in preparation for fixing energy balances
  IEAData = target(extract_country_data(AllIEAData, countries, max_year), dynamic = map(countries)),
  
  # Check whether energy products are balanced. They're not. 
  # FALSE indicates a country with at least one balance problem.
  BalancedBefore = target(is_balanced(IEAData, countries), dynamic = map(countries)),
  # Balance all of the data by product and year.
  BalancedIEAData = target(make_balanced(IEAData, countries), dynamic = map(countries)),
  # Check that everything is balanced after balancing.
  BalancedAfter = target(is_balanced(BalancedIEAData, countries), dynamic = map(countries)),
  # Don't continue if there is a problem.
  OKToProceed = stopifnot(all(BalancedAfter)),
  
  # Specify the BalancedIEAData data frame by being more careful with names, etc.
  Specified = target(specify(BalancedIEAData, countries), dynamic = map(countries)),
  
  # Arrange all the data into PSUT matrices.
  PSUT = target(make_psut(Specified, countries), dynamic = map(countries))
  
  
)
