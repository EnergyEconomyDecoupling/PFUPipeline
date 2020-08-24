library(readr)

# Imports IEA WEEB Data for 2019 and 2020. Uncomment if required.
IEA_Extended_Energy_Balances_2019 <- read_csv("C:/Users/Zeke Marshall/Dropbox/Fellowship 1960-2015 PFU database/IEA extended energy balance data/IEA 2019 energy balance data/IEA Extended Energy Balances 2019.csv")
IEA_Extended_Energy_Balances_2020 <- read_csv("C:/Users/Zeke Marshall/Dropbox/Fellowship 1960-2015 PFU database/IEA extended energy balance data/IEA 2020 energy balance data/IEA Extended Energy Balances 2020.csv")

# Creates data frames for the column names of the IEA EEB data 
colnames_2020 <- as.data.frame(colnames(IEA_Extended_Energy_Balances_2020))
colnames_2019 <- as.data.frame(colnames(IEA_Extended_Energy_Balances_2019))

# Creates data frames containing the unique countries
countries_2020 <- as.data.frame(unique(IEA_Extended_Energy_Balances_2020$COUNTRY))
colnames(countries_2020) <- "COUNTRY"
  
countries_2019 <- as.data.frame(unique(IEA_Extended_Energy_Balances_2019$COUNTRY))
colnames(countries_2019) <- "COUNTRY"

# Creates a DF with countries which do not occur in 2020
countries_not_in_2020 <- dplyr::anti_join(countries_2019, countries_2020, by = NULL)
colnames(countries_not_in_2020) <- "countries_not_in_2020"

# Creates a DF with countries which do not occur in 2019
countries_not_in_2019 <- dplyr::anti_join(countries_2020, countries_2019, by = NULL)
colnames(countries_not_in_2019) <- "countries_not_in_2019"

# Creates data frames containing the unique products
products_2020 <- as.data.frame(unique(IEA_Extended_Energy_Balances_2020$PRODUCT))
colnames(products_2020) <- "PRODUCT"

products_2019 <- as.data.frame(unique(IEA_Extended_Energy_Balances_2019$PRODUCT))
colnames(products_2019) <- "PRODUCT"

# Creates a DF with products which do not occur in 2020
products_not_in_2020 <- dplyr::anti_join(products_2019, products_2020, by = NULL)
colnames(products_not_in_2020) <- "products_not_in_2020"

# Creates a DF with products which do not occur in 2019
products_not_in_2019 <- dplyr::anti_join(products_2020, products_2019, by = NULL)
colnames(products_not_in_2019) <- "products_not_in_2019"

# Creates data frames containing the unique flows
flows_2020 <- as.data.frame(unique(IEA_Extended_Energy_Balances_2020$FLOW))
colnames(flows_2020) <- "FLOW"

flows_2019 <- as.data.frame(unique(IEA_Extended_Energy_Balances_2019$FLOW))
colnames(flows_2019) <- "FLOW"

# Creates a DF with flows which do not occur in 2020
flows_not_in_2020 <- dplyr::anti_join(flows_2019, flows_2020, by = NULL)
colnames(flows_not_in_2020) <- "flows_not_in_2020"

# Creates a DF with products which do not occur in 2019
flows_not_in_2019 <- dplyr::anti_join(flows_2020, flows_2019, by = NULL)
colnames(flows_not_in_2019) <- "flows_not_in_2019"

# Creates a summary data frame with all differences. 
# I need to do this in a less crude manner

differences_summary <- countries_not_in_2020 %>%
  merge(countries_not_in_2019, by='row.names', all=TRUE) %>%
  merge(products_not_in_2020, by='row.names', all=TRUE) %>%
  merge(products_not_in_2019, by='row.names', all=TRUE) %>%
  merge(flows_not_in_2020, by='row.names', all=TRUE) %>%
  merge(flows_not_in_2019, by='row.names', all=TRUE)

differences_summary[,1:5] <- NULL

