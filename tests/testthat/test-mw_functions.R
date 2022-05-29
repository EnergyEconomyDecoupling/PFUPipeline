test_that("rename_mw_sectors() works as expected", {
  df <- tibble::tribble(~Sector, ~value, 
                        MWTools::mw_sectors$agriculture_broad.sector, 10,
                        MWTools::mw_sectors$transport_sector,         11,
                        MWTools::mw_sectors$services_broad.sector,    12,
                        MWTools::mw_sectors$industry_broad.sector,    13, 
                        "bogus",                                      14)
  
  res <- df %>% 
    rename_mw_sectors()
  
  expect_equal(res[["Sector"]], c(IEATools::other_flows$agriculture_forestry, 
                                  IEATools::transport_flows$transport_not_elsewhere_specified, 
                                  IEATools::other_flows$commercial_and_public_services, 
                                  IEATools::industry_flows$industry_not_elsewhere_specified, 
                                  "bogus"))
})
