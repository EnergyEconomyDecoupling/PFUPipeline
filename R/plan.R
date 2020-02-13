# The workflow plan data frame outlines what you are going to do.

plan <- drake_plan(
  countries = c("ESP", "USA"),
  paths = get_abs_paths(), 
  AllIEAData = load_IEA_data(paths$oecd_path, paths$nonoecd_path), 
  CountryData = target(extract_country_data(AllIEAData, countries), dynamic = map(countries, .trace = countries))
)
