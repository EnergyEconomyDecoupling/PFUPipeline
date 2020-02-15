# The workflow plan data frame outlines the analyses to be conducted.

plan <- drake_plan(
  countries = c("ESP"),
  paths = get_abs_paths(), 
  AllIEAData = load_tidy_iea_df(paths$iea_data_path),
  CountryIEAData = target(extract_country_data(AllIEAData, countries), dynamic = map(countries, .trace = countries))
)
