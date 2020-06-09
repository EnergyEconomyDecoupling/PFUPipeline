# Generate simplified mapping information (using ESP as an example)

# Read a ESP Fu Allocations table for a particular country

ESP_FU_Allocations_Table <- readxl::read_excel("C:/Users/Zeke Marshall/Dropbox/Fellowship 1960-2015 PFU database/Country-level exergy accounting data/ESP/ESP FU Analysis.xlsx")

# Read the unique Ef.product, Destination, Machine and Eu.product combinations from an FU Allocations table

ESP_mapping <- unique(ESP_FU_Allocations_Table[,c('Ef.product','Destination', 'Machine', 'Eu.product')]) 

# Omit rows containing NA

ESP_mapping <- na.omit(ESP_mapping)

# Reorder columns so the order becomes Destination, Ef.product, Machine, Eu.product

ESP_mapping <- ESP_mapping[,c(2,1,3,4)]

# Remove the all but the first instance of every entry?


# View mapping info

View(ESP_mapping)

### Export DF as a .pdf .docx??
