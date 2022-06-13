# This script creates constants and saves them in the right locations.
# If there are any changes to these constants,
# source this script before building the package.

library(magrittr)
library(IEATools)

#
# Names of targets
#

target_names <- list(countries = "Countries",
                     additinoal_exemplar_countries = "AdditionalExemplarCountries",
                     alloc_and_eff_countries = "AllocAndEffCountries",
                     years = "Years",
                     iea_data_path = "IEADataPath",
                     country_concordance_path = "CountryConcordancePath",
                     phi_constants_path = "PhiConstantsPath",
                     ceda_data_folder = "CEDADataFolder",
                     machine_data_path = "MachineDataPath",
                     exemplar_table_path = "ExemplarTablePath",
                     fu_analysis_folder = "FUAnalysisFolder",
                     report_source_folders = "ReportsSourceFolders",
                     report_dest_folder = "ReportsDestFolder",
                     pipeline_caches_folder = "PipelineCachesFolder",
                     pipeline_releases_folder = "PipelineReleasesFolder",
                     release = "Release", 
                     iea_data = "IEAData",
                     country_concordance_table = "CountryConcordanceTable",
                     ceda_data = "CEDAData",
                     all_machine_data = "AllMachineData",
                     machine_data = "MachineData",
                     socio_econ_data = "SocioEconData",
                     balanced_before = "BalancedBefore",
                     balanced_iea_data = "BalancedIEAData",
                     balanced_after = "BalancedAfter",
                     ok_to_proceed = "OKToProceed",
                     specified = "Specified",
                     psut_final = "PSUTFinal",
                     exemplar_lists = "ExemplarLists",
                     phi_constants = "PhiConstants",
                     incomplete_allocation_tables = "IncompleteAllocationTables",
                     completed_allocation_tables = "CompletedAllocationTables",
                     completed_efficiency_tables = "CompletedEfficiencyTables",
                     completed_phi_u_tables = "CompletedPhiuTables",
                     cmats = "Cmats",
                     eta_fu_phi_u_vecs = "EtafuPhiuvecs",
                     eta_fu_vecs = "Etafuvecs",
                     phi_u_vecs = "Phiuvecs",
                     phi_pf_vecs = "Phipfvecs",
                     phi_vecs = "Phivecs",
                     psut_useful = "PSUTUseful",
                     psut = "PSUT",
                     allocation_graphs = "AllocationGraphs",
                     non_stationary_allocation_graphs = "NonStationaryAllocationGraphs",
                     efficiency_graphs = "EfficiencyGraphs",
                     phi_graphs = "PhiGraphs",
                     release_psut = "ReleasePSUT", 
                     store_cache = "StoreCache")
usethis::use_data(target_names, overwrite = TRUE)


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


#
# All countries to run in the workflow
#

all_countries <- list(
  afri = "AFRI",
  ago = "AGO",
  alb = "ALB",
  are = "ARE",
  arg = "ARG",
  arm = "ARM",
  asia = "ASIA",
  aus = "AUS",
  aut = "AUT",
  aze = "AZE",
  bel = "BEL",
  ben = "BEN",
  bfa = "BFA",
  bgd = "BGD",
  bgr = "BGR",
  bhr = "BHR",
  bih = "BIH",
  blr = "BLR",
  bol = "BOL",
  bra = "BRA",
  brn = "BRN",
  bwa = "BWA",
  bunk = "BUNK",
  can = "CAN",
  che = "CHE",
  chl = "CHL",
  chnm = "CHNM",
  cmr = "CMR",
  cod = "COD",
  cog = "COG",
  col = "COL",
  civ = "CIV",
  cri = "CRI",
  cub = "CUB",
  cuw = "CUW",
  cyp = "CYP",
  cze = "CZE",
  deu = "DEU",
  dnk = "DNK",
  dom = "DOM",
  dza = "DZA",
  ecu = "ECU",
  egy = "EGY",
  eri = "ERI",
  esp = "ESP",
  est = "EST",
  eth = "ETH",
  eurp = "EURP",
  fin = "FIN",
  fra = "FRA",
  gab = "GAB",
  gbr = "GBR",
  geo = "GEO",
  gha = "GHA",
  gib = "GIB",
  gnq = "GNQ",
  grc = "GRC",
  grl = "GRL",
  gtm = "GTM",
  guy = "GUY",
  hkg = "HKG",
  hnd = "HND",
  hrv = "HRV",
  hti = "HTI",
  hun = "HUN",
  idn = "IDN",
  ind = "IND",
  irl = "IRL",
  irn = "IRN",
  irq = "IRQ",
  isl = "ISL",
  isr = "ISR",
  ita = "ITA",
  jam = "JAM",
  jor = "JOR",
  jpn = "JPN",
  kaz = "KAZ",
  ken = "KEN",
  kgz = "KGZ",
  khm = "KHM",
  kor = "KOR",
  kwt = "KWT",
  lao = "LAO",
  lbn = "LBN",
  lby = "LBY",
  lka = "LKA",
  ltu = "LTU",
  lux = "LUX",
  lva = "LVA",
  mar = "MAR",
  mda = "MDA",
  mdg = "MDG",
  mex = "MEX",
  mide = "MIDE",
  msu = "MSU",
  mkd = "MKD",
  mli = "MLI",
  mlt = "MLT",
  mmr = "MMR",
  mne = "MNE",
  mng = "MNG",
  moz = "MOZ",
  mrt = "MRT",
  mus = "MUS",
  myu = "MYU",
  mys = "MYS",
  nam = "NAM",
  namr = "NAMR",
  ner = "NER",
  nga = "NGA",
  nic = "NIC",
  nld = "NLD",
  nor = "NOR",
  npl = "NPL",
  nzl = "NZL",
  oafr = "OAFR",
  oasi = "OASI",
  oamr = "OAMR",
  ocen = "OCEN",
  omn = "OMN",
  pak = "PAK",
  pan = "PAN",
  per = "PER",
  phl = "PHL",
  pol = "POL",
  prk = "PRK",
  prt = "PRT",
  pry = "PRY",
  pse = "PSE",
  qat = "QAT",
  rou = "ROU",
  rus = "RUS",
  rwa = "RWA",
  samr = "SAMR",
  sau = "SAU",
  sdn = "SDN",
  sen = "SEN",
  sgp = "SGP",
  slv = "SLV",
  srb = "SRB",
  ssd = "SSD",
  sun = "SUN",
  sur = "SUR",
  svk = "SVK",
  svn = "SVN",
  swe = "SWE",
  syr = "SYR",
  tcd = "TCD",
  tgo = "TGO",
  tha = "THA",
  tjk = "TJK",
  tkm = "TKM",
  tto = "TTO",
  tun = "TUN",
  tur = "TUR",
  twn = "TWN",
  tza = "TZA",
  uga = "UGA",
  ukr = "UKR",
  ury = "URY",
  usa = "USA",
  uzb = "UZB",
  ven = "VEN",
  vnm = "VNM",
  wabk = "WABK",
  wrld = "WRLD",
  wmbk = "WMBK",
  xkx = "XKX",
  yem = "YEM",
  yug = "YUG",
  zaf = "ZAF",
  zmb = "ZMB",
  zwe = "ZWE"
)

usethis::use_data(all_countries, overwrite = TRUE)


#
# Countries whose data also exists in another 'country';
# e.g., Memo: Uganda (UGA)
# in Other Africa (OAF).
#

double_counted_countries <- list(
  afri = "AFRI",
  asia = "ASIA",
  bfa = "BFA",
  bunk = "BUNK",
  eurp = "EURP",
  mdg = "MDG",
  mide = "MIDE",
  mli = "MLI",
  mrt = "MRT",
  msu = "MSU",
  myu = "MYU",
  namr = "NAMR",
  rwa = "RWA",
  ocen = "OCEN",
  samr = "SAMR",
  tcd = "TCD",
  uga = "UGA",
  wrld = "WRLD"
)

usethis::use_data(double_counted_countries, overwrite = TRUE)


#
# Countries to run in the workflow which should sum to World (WRLD)
#

canonical_countries <- dplyr::setdiff(all_countries,
                                      double_counted_countries)

usethis::use_data(canonical_countries, overwrite = TRUE)


#
# Names and constants associated with exemplar tables.
#

exemplar_names <- list(exemplar_tab_name = "exemplar_table",
                       prev_names = "Prev.names",
                       exemplars = "Exemplars",
                       exemplar_country = "Exemplar.country",
                       exemplar_countries = "Exemplar.countries",
                       exemplar_tables = "Exemplar.tables",
                       iea_data = "IEA.data",
                       alloc_data = "Alloc.data",
                       incomplete_alloc_table = "Incomplete.alloc.table",
                       complete_alloc_table = "Complete.alloc.table",
                       incomplete_eta_table = "Incomplete.eta.table",
                       complete_eta_table = "Complete.eta.table",
                       region_code = "Region.code",
                       country_name = "Country.name",
                       agg_code_col = "Agg.Code",
                       world = "WRLD")
usethis::use_data(exemplar_names, overwrite = TRUE)


#
# phi.sources
#

phi_sources <- list(eta_fu_tables = "eta_fu.tables",
                    temperature_data = "temperature.data",
                    phi_constants = "phi.constants")
usethis::use_data(phi_sources, overwrite = TRUE)
