# This script creates constants and saves them in the right locations.
# If there are any changes to these constants,
# source this script before building the package.

library(magrittr)
library(IEATools)


#
# Tab name for machine efficiencies
#

machine_constants <- list(efficiency_tab_name = "FIN_ETA")
usethis::use_data(machine_constants, overwrite = TRUE)


#
# Column names for socio-economic data
#

socioecon_cols <- list(isocode_colname = "isocode",
                       year_colname = "year",
                       rgdpe_colname = "rgdpe",
                       rgdpo_colname = "rgdpo",
                       rgdpna_colname = "rgdpna",
                       emp_colname = "emp",
                       avh_colname = "avh",
                       hc_colname = "hc",
                       rnna_colname = "rnna",
                       rkna_colname = "rkna",
                       K_colname = "K",
                       Kserv_colname = "Kserv",
                       L_colname = "L",
                       Ladj_colname = "Ladj")
usethis::use_data(socioecon_cols, overwrite = TRUE)