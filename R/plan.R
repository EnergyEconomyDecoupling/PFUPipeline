# The workflow plan data frame outlines the analyses to be conducted.

plan <- drake_plan(

  countries = c("CHN","ESP","GHA","GRC","ISR","MAR","PRT","World"),
  max_year = 2017,
  paths = get_abs_paths(),
  
  # (1) Grab all the IEA data for ALL countries
  AllIEAData = load_tidy_iea_df(paths$iea_data_path),
  
  # Narrow down to only the countries of interest to us
  # and group in preparation for fixing energy balances
  IEAData = target(extract_country_data(AllIEAData, countries, max_year), dynamic = map(countries)),
  
  # (2) Balance all the energy data. 
  # First, check whether energy products are balanced. They're not. 
  # FALSE indicates a country with at least one balance problem.
  balanced_before = target(is_balanced(IEAData, countries), dynamic = map(countries)),
  # Balance all of the data by product and year.
  BalancedIEAData = target(make_balanced(IEAData, countries), dynamic = map(countries)),
  # Check that everything is balanced after balancing.
  balanced_after = target(is_balanced(BalancedIEAData, countries), dynamic = map(countries)),
  # Don't continue if there is a problem.
  OKToProceed = stopifnot(all(balanced_after)),
  
  # (3) Specify the BalancedIEAData data frame by being more careful with names, etc.
  Specified = target(specify(BalancedIEAData, countries), dynamic = map(countries)),
  
  # (4) Arrange all the data into PSUT matrices with final stage data.
  PSUT_final = target(make_psut(Specified, countries), dynamic = map(countries))
  
  # (5) Load allocation tables
  
  
  # (6) Load efficiency tables
  
  
  # (7) Extend to useful stage.  
  
  
)
