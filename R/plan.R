# The workflow plan data frame outlines the analyses to be conducted.

plan <- drake_plan(
  countries = c("ESP", "USA", "CAN"),
  paths = get_abs_paths(), 
  AllIEAData = load_IEA_data(paths$oecd_path, paths$nonoecd_path), 
  CountryIEAData = target(extract_country_data(AllIEAData, countries), dynamic = map(countries, .trace = countries))
)
