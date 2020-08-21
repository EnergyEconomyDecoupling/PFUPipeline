# This function collates all FU etas sheets from completed countries 
# and combines them into a data frame

source("OddWork/fu_analysis_path.R")

# Creates a function which reads a FU Analysis file and extracts the etas time-series

allocations_func <- function(country_path) {
  readxl::read_excel(country_path, sheet = "FU Allocations")
}

# Applies the etas_func function to each file within the list and then binds the data into a single data frame

allocations_data <- lapply(analysis_files_list, allocations_func) %>%
  dplyr::bind_rows()

# Re-orders columns
allocations_data <- allocations_data[ , c("Country", 	"Method", "Energy.type",	"Last.stage",	"Ledger.side", "Unit", "Ef.product",	
                                          "Machine",	"Eu.product", "Destination",	"Quantity",	"Maximum.values",	
                                          "1960",	"1961",	"1962",	"1963",	"1964",	"1965",	"1966",	
                                          "1967",	"1968",	"1969",	"1970",	"1971",	"1972",	"1973",	"1974",	"1975",	"1976",	"1977",	
                                          "1978",	"1979",	"1980",	"1981",	"1982",	"1983",	"1984",	"1985",	"1986",	"1987",	"1988",	
                                          "1989",	"1990",	"1991",	"1992",	"1993",	"1994", "1995",	"1996",	"1997",	"1998",	"1999",	
                                          "2000",	"2001",	"2002",	"2003",	"2004",	"2005",	"2006",	"2007",	"2008",	"2009",	"2010",	
                                          "2011",	"2012",	"2013",	"2014",	"2015",	"2016",	"2017")]

# Filter the data DF to only include allocations data
# allocations_data <- dplyr::filter(allocations_data, allocations_data$Quantity == c("eta.fu"))

# Removes entries in Machine with no data
allocations_data <- allocations_data %>% dplyr::filter(!is.na(Machine))

# Re-organises allocations_data into panel data format
allocations_data <- reshape2::melt(allocations_data, 
                            id.vars = c("Country", 	"Method", "Energy.type",	"Last.stage",	"Ledger.side", "Unit", "Ef.product",	
                                        "Machine",	"Eu.product", "Destination",	"Quantity",	"Maximum.values")
                                        )

# Re-names column "variable" to "Year"
names(allocations_data)[names(allocations_data) == 'variable'] <- 'Year'

# Re-names column "value" to "Allocation"
names(allocations_data)[names(allocations_data) == 'value'] <- 'Allocation'


# Writes a .csv file for the allocations_data DF
write.csv(allocations_data, file = "C:/Github/PFU-Database/OddWork/allocations_data.csv")

r# Groups etas_data DF by machine rather than country

# m_etas_data <- dplyr::arrange(etas_data, Machine)

# Writes a .csv file for the m_etas_data DF
# write.csv(m_etas_data, file = "C:/Github/PFU-Database/OddWork/m_etas_data.csv")

# OR groups etas_data DF by Eu.product 

# Eu_etas_data <- dplyr::arrange(etas_data, Eu.product)

# Generates a list of unique machines
machines <- unique(etas_data$Machine)

# Generates a list of unique products
eu.products <- unique(etas_data$Eu.product)

                        
                      
                      


