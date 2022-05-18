test_that("calc_phi_vecs_mw() works as expected", {
  mw_psut_df <- tibble::tribble(~Country, ~Year, 
                                "USA", 1971, 
                                "GBR", 1972, 
                                "GHA", 1973, 
                                "ZAF", 1974)
  mw_phi <- MWTools::phi_vec_mw()
  res <- calc_phi_vecs_mw(psut_energy_mw = mw_psut_df, 
                          phi_vec_mw = mw_phi, 
                          countries = c("USA", "GBR", "GHA", "ZAF"))
  for (i in 1:nrow(res)) {
    expect_equal(res[["phi"]][[i]], mw_phi)
  }
})
