# Reads the FU allocations template for ESP

ESP_FU_Allocations_Template <- readxl::read_excel("C:/Users/Zeke Marshall/Dropbox/Fellowship 1960-2015 PFU database/Country-level exergy accounting data/ESP/Details/ESP FU Allocations Template.xlsx")

# Identifies unique Ef.product and Destination combinations from ESP

ESP_uniq_comb <- unique(ESP_FU_Allocations_Template[,c('Ef.product','Destination')])
# View(ESP_uniq_comb)

# Reads the FU allocations template for GRC

GRC_FU_Allocations_Template <- readxl::read_excel("C:/Users/Zeke Marshall/Dropbox/Fellowship 1960-2015 PFU database/Country-level exergy accounting data/GRC/GRC FU Allocations Template.xlsx")

# Identifies unique Ef.product and Destination combinations from GRC

GRC_uniq_comb <- unique(GRC_FU_Allocations_Template[,c('Ef.product','Destination')])
# View(GRC_uniq_comb)

# Identifies the combinations present in GRC but not in ESP

in_GRC_not_ESP <- dplyr::anti_join(GRC_uniq_comb, ESP_uniq_comb)
View(in_GRC_not_ESP)

# Identifies the combinations present in ESP but not in GRC

in_ESP_not_GRC <- dplyr::anti_join(ESP_uniq_comb, GRC_uniq_comb)
View(in_ESP_not_GRC)