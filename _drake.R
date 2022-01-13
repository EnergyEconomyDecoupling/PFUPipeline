# This file serves the r_*() functions (e.g. r_make()) documented at
# https://books.ropensci.org/drake/projects.html#safer-interactivity # nolint
# and
# https://docs.ropensci.org/drake/reference/r_make.html

# Load your packages and supporting functions into your session.
# If you use supporting scripts like the ones below,
# you will need to supply them yourself. Examples:
# https://github.com/wlandau/drake-examples/tree/master/main/R

library(SEAPSUTWorkflow)

# Custom parameters
max_year <- 2019                         # The last year to be analyzed

# countries <- c("BRA", "CAN", "CHN", "DEU", "ESP", "GBR", "GHA", "GRC", "HKG", "HND", "IDN", "JPN", "IND", "JOR", "KOR", "MEX", "NOR", "RUS", "USA", "WMB", "WAB", "ZAF")
# countries <- c("WMB")
# countries <- c("USA")
# countries <- c("FSU", "YGS")
# countries <- c("SUN", "YUG")
# countries <- c("YGS")
# countries <- setdiff(SEAPSUTWorkflow::canonical_countries, c("FSU", "FYG", "CIV")) |> as.character()
countries <- setdiff(SEAPSUTWorkflow::canonical_countries, c("CIV", "CUW", "WLD")) |> as.character()

# countries <- list(
#   ago = "AGO",
#   alb = "ALB",
#   are = "ARE",
#   arg = "ARG",
#   arm = "ARM",
#   aus = "AUS",
#   aut = "AUT",
#   aze = "AZE",
#   bel = "BEL",
#   ben = "BEN",
#   bfa = "BFA",
#   bgd = "BGD",
#   bgr = "BGR",
#   bhr = "BHR",
#   bih = "BIH",
#   blr = "BLR",
#   bol = "BOL",
#   bra = "BRA",
#   brn = "BRN",
#   bwa = "BWA",
#   can = "CAN",
#   che = "CHE",
#   chl = "CHL",
#   chn = "CHN",
#   cmr = "CMR",
#   cod = "COD",
#   cog = "COG",
#   col = "COL",
#   # civ = "CIV",   # Fails: No IEA data.
#   cri = "CRI",
#   cub = "CUB",
#   # cuw = "CUW",   # Fails: No IEA data.
#   cyp = "CYP",
#   cze = "CZE",
#   deu = "DEU",
#   dnk = "DNK",
#   dom = "DOM",
#   dza = "DZA",
#   ecu = "ECU",
#   egy = "EGY",
#   eri = "ERI",
#   esp = "ESP",
#   est = "EST",
#   eth = "ETH",
#   fin = "FIN",
#   fra = "FRA",
#   # fsu = "FSU",   # Fails for unknown reasons.
#   # fyg = "FYG",   # Fails for unknown reasons.
#   gab = "GAB",
#   gbr = "GBR",
#   gbr = "GEO",
#   gha = "GHA",
#   gib = "GIB",
#   gnq = "GNQ",
#   grc = "GRC",
#   grl = "GRL",
#   gtm = "GTM",
#   guy = "GUY",
#   hkg = "HKG",
#   hnd = "HND",
#   hrv = "HRV",
#   hti = "HTI",
#   hun = "HUN",
#   idn = "IDN",
#   ind = "IND",
#   irl = "IRL",
#   irn = "IRN",
#   irq = "IRQ",
#   isl = "ISL",
#   isr = "ISR",
#   ita = "ITA",
#   jam = "JAM",
#   jor = "JOR",
#   jpn = "JPN",
#   kaz = "KAZ",
#   ken = "KEN",
#   kgz = "KGZ",
#   khm = "KHM",
#   kor = "KOR",
#   kwt = "KWT",
#   lao = "LAO",
#   lbn = "LBN",
#   lby = "LBY",
#   lka = "LKA",
#   ltu = "LTU",
#   lux = "LUX",
#   lva = "LVA",
#   mar = "MAR",
#   mda = "MDA",
#   mdg = "MDG",
#   mex = "MEX",
#   mfs = "MFS",
#   mkd = "MKD",
#   mli = "MLI",
#   mlt = "MLT",
#   mmr = "MMR",
#   mne = "MNE",
#   mng = "MNG",
#   moz = "MOZ",
#   mrt = "MRT",
#   mus = "MUS",
#   myg = "MYG",
#   mys = "MYS",
#   nam = "NAM",
#   ner = "NER",
#   nga = "NGA",
#   nic = "NIC",
#   nld = "NLD",
#   nor = "NOR",
#   npl = "NPL",
#   nzl = "NZL",
#   oaf = "OAF",
#   oam = "OAM",
#   oas = "OAS",
#   omn = "OMN",
#   pak = "PAK",
#   pan = "PAN",
#   per = "PER",
#   phl = "PHL",
#   pol = "POL",
#   prk = "PRK",
#   prt = "PRT",
#   pry = "PRY",
#   pse = "PSE",
#   qat = "QAT",
#   rou = "ROU",
#   rus = "RUS",
#   rwa = "RWA",
#   sau = "SAU",
#   sdn = "SDN",
#   sen = "SEN",
#   sgp = "SGP",
#   slv = "SLV",
#   srb = "SRB",
#   ssd = "SSD",
#   sur = "SUR",
#   svk = "SVK",
#   svn = "SVN",
#   swe = "SWE",
#   syr = "SYR",
#   tcd = "TCD",
#   tgo = "TGO",
#   tha = "THA",
#   tjk = "TJK",
#   tkm = "TKM",
#   tto = "TTO",
#   tun = "TUN",
#   tur = "TUR",
#   twn = "TWN",
#   tza = "TZA",
#   uga = "UGA",
#   ukr = "UKR",
#   ury = "URY",
#   usa = "USA",
#   uzb = "UZB",
#   ven = "VEN",
#   vnm = "VNM",
#   wab = "WAB",
#   
#   # wld = "WLD"    # Causes a stack too large error.  Maybe getting into a weird infinite loop?
#                    # Perhaps WLD should not be its own exemmplar?
#   
#   wmb = "WMB",
#   xkx = "XKX",
#   yem = "YEM",
#   zaf = "ZAF",
#   zmb = "ZMB",
#   zwe = "ZWE"
# )



additional_exemplars <- "WLD"

# Create our drake plan
plan <- SEAPSUTWorkflow::get_plan(countries = countries, 
                                  additional_exemplar_countries = additional_exemplars,
                                  max_year = max_year,
                                  iea_data_path = PFUSetup::get_abs_paths()[["iea_data_path"]],
                                  country_concordance_path = PFUSetup::get_abs_paths()[["country_concordance_path"]],
                                  phi_constants_path = PFUSetup::get_abs_paths()[["phi_constants_path"]],
                                  ceda_data_folder = PFUSetup::get_abs_paths()[["ceda_data_folder"]],
                                  machine_data_path = PFUSetup::get_abs_paths()[["machine_data_folder"]],
                                  exemplar_table_path = PFUSetup::get_abs_paths()[["exemplar_table_path"]],
                                  fu_analysis_folder = PFUSetup::get_abs_paths()[["fu_analysis_folder"]],
                                  reports_source_folders = PFUSetup::get_abs_paths()[["reports_source_folders"]],
                                  reports_dest_folder = PFUSetup::get_abs_paths()[["reports_dest_folder"]])




# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
options(clustermq.scheduler = "multicore") # For parallel computing.
drake::drake_config(
  plan, 
  # max_expand = 1 # Set the number of countries you want to analyze
  parallelism = "clustermq",
  jobs = 8
)
