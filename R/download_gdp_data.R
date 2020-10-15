# Loads required packages
library(wbstats)
library(tidyverse)

################################################################################
## WORLD BANK

# Interestingly trying to download data with wbstats::wb_data() fails with the 
# error code: Error: World Bank API request failed for indicator SL.TLF.ACTI.ZS
#             message: Server error: (502) Bad Gateway
#             category: Server error
#             reason: Bad Gateway 
# but using wb_data() succeeds.

# Provides a snapshot of available countries, indicators, and other relevant information
str(wb_cachelist, max.level = 1)

# Updates World Bank API cache, stores in list
new_cache <- wb_cache()

# Searches for indicators containing relevant terms
gdp_inds <- wb_search("GDP")

# Creates a dataframe containing GDP in constant 2010 US dollars by country
gdp_const2010_us <- wb_data("NY.GDP.MKTP.KD") %>%
  dplyr::select(2:5) %>%
  magrittr::set_colnames(c("ISO_Country_Code", "Country", "Year", "GDP_2010_US"))
