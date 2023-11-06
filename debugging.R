

# In this script we build the code to calculate the EIOU-wide, Y-wide, and economy-wide, C matrices.

# Loading data
tar_load(Cmats)
tar_load(PSUTIEA)
tar_load(Etafuvecs)

# Checking data
# print(Cmats)
# print(Etafuvecs)
# print(PSUTIEA)
# 
# View(Cmats$C_EIOU[[1]] |> as.matrix())
# View(Etafuvecs$eta.fu[[1]] |> as.matrix())
# View(PSUTIEA$Y[[1]] |> as.matrix())
# View(PSUTIEA$U_EIOU[[1]] |> as.matrix())

# Testing first with dplyr

# This here does not work for some reason
# C_trimmed <- Cmats |> 
#   mutate(
#     test = matsbyname::vectorize_byname(a = C_EIOU, notation = RCLabels::arrow_notation)
#   ) |> 
#   print()




Cmatsagg <- dplyr::left_join(
  Cmats, PSUTIEA, by = c("Country", "Method", "Energy.type", "Last.stage", "Year")
) |> 
  dplyr::select(Country, Method, Energy.type, Last.stage, Year, C_EIOU, C_Y, Y, U_EIOU) |> 
  # Calculating all the bunch of vectors and matrices needed now:
  dplyr::mutate(
    # Total use of product p industry EIOU industry i, as a vector. U_EIOU vectorised.
    eiou_vec = matsbyname::vectorize_byname(a = U_EIOU, notation = list(RCLabels::arrow_notation)),
    # Total use of product p in final demand sector s, as a vector. Y vectorised.
    y_vec = matsbyname::vectorize_byname(a = Y, notation = list(RCLabels::arrow_notation)),
    # Total use of product p in machine m across all EIOU industries aggregated
    Alloc_mat_EIOU = matsbyname::matrixproduct_byname(matsbyname::hatize_byname(eiou_vec),
                                                      C_EIOU) |> 
      matsbyname::aggregate_pieces_byname(piece = "noun", 
                                          margin = 1, 
                                          notation = list(RCLabels::arrow_notation)),
    # Total use of product p in final demand sector s across all final demand sectors aggregated
    Alloc_mat_Y = matsbyname::matrixproduct_byname(matsbyname::hatize_byname(y_vec),
                                                   C_Y) |> 
      matsbyname::aggregate_pieces_byname(piece = "noun", 
                                          margin = 1, 
                                          notation = list(RCLabels::arrow_notation)),
    # Total use of product p in machine m Y and EIOU aggregated
    Alloc_mat_EIOU_Y = matsbyname::sum_byname(Alloc_mat_EIOU, Alloc_mat_Y),
    # Total use of product p in EIOU industries
    f_EIOU = matsbyname::rowsums_byname(Alloc_mat_EIOU),
    # Total use of product p in Y
    f_Y = matsbyname::rowsums_byname(Alloc_mat_Y),
    # Total use of product p in EIOU and Y
    f_EIOU_Y = matsbyname::rowsums_byname(Alloc_mat_EIOU_Y),
    # Share of product p used in each machine m across EIOU. Sum by product yields 1.
    C_EIOU_agg = matsbyname::matrixproduct_byname(matsbyname::hatinv_byname(f_EIOU, keep = "rownames"),
                                                       Alloc_mat_EIOU),
    # Share of product p used in each machine m across final demand. Sum by product yields 1.
    C_Y_agg = matsbyname::matrixproduct_byname(matsbyname::hatinv_byname(f_Y),
                                                    Alloc_mat_Y),
    # Share of product p used in each machine m across final demand and EIOU. Sum by product yields 1.
    C_EIOU_Y_agg = matsbyname::matrixproduct_byname(matsbyname::hatinv_byname(f_EIOU_Y, keep = "rownames"),
                                                         Alloc_mat_EIOU_Y),
  ) |> 
  dplyr::select(Country, Method, Energy.type, Last.stage, Year, C_EIOU_agg, C_Y_agg, C_EIOU_Y_agg) |> 
  glimpse()


# Need to compare results with the other results now.


# Testing now with individual matrices

C_EIOU <- as.matrix(Cmats$C_EIOU[[1]])
U_EIOU <- as.matrix(PSUTIEA$U_EIOU[[1]])

C_Y <- as.matrix(Cmats$C_Y[[1]])
Y <- as.matrix(PSUTIEA$Y[[1]])

eta_i <- as.matrix(Etafuvecs$eta.fu[[1]])

#View(as.matrix(C_EIOU))

# Building eiou_vec and y_vec
eiou_vec <- matsbyname::vectorize_byname(a = U_EIOU, notation = RCLabels::arrow_notation) #|> 
  #matsbyname::setcolnames_byname("Total")
y_vec <- matsbyname::vectorize_byname(a = Y, notation = RCLabels::arrow_notation) #|> 
  #matsbyname::setcolnames_byname("Total")

# Got this working so we should now be able to do the (Y,EIOU) aggregated version.
# Also not needed apparently
# y_eiou_vec <- matsbyname::sum_byname(eiou_vec, y_vec)


# Calculating transp(C_EIOU)*eiou_vec transp(C)*y_vec -> gives the allocation matrix

# Allocation EIOU energy use
Alloc_eiou <- matsbyname::matrixproduct_byname(
  matsbyname::hatize_byname(eiou_vec),
  C_EIOU
) |> 
  matsbyname::aggregate_pieces_byname(piece = "noun", 
                                      margin = 1, 
                                      notation = RCLabels::arrow_notation)

# Allocation Y energy use
Alloc_Y <- matsbyname::matrixproduct_byname(
  matsbyname::hatize_byname(y_vec),
  C_Y
) |> 
  matsbyname::aggregate_pieces_byname(piece = "noun", 
                                      margin = 1, 
                                      notation = RCLabels::arrow_notation)

Alloc_EIOU_Y <- matsbyname::sum_byname(Alloc_eiou, Alloc_Y)


# Building f vector
f_eiou <- matsbyname::rowsums_byname(Alloc_eiou)
f_Y <- matsbyname::rowsums_byname(Alloc_Y)

f_eiou_Y <- matsbyname::rowsums_byname(Alloc_EIOU_Y)

# Building C matrices now
C_agg_EIOU <- matsbyname::matrixproduct_byname(
  matsbyname::hatinv_byname(f_eiou),
  Alloc_eiou
)

View(C_agg_EIOU)

C_agg_Y <- matsbyname::matrixproduct_byname(
  matsbyname::hatinv_byname(f_Y),
  Alloc_Y
)

View(C_agg_Y)

C_agg_EIOU_Y <- matsbyname::matrixproduct_byname(
  matsbyname::hatinv_byname(f_eiou_Y),
  Alloc_EIOU_Y
)

# Tests on C_agg matrices

# Careful there are some zeros as well! Investigate this!

# C_agg_EIOU |> matsbyname::rowsums_byname() |> View()
# C_agg_Y |> matsbyname::rowsums_byname() |> View()
# C_agg_EIOU_Y |> matsbyname::rowsums_byname() |> View()


# Comparing results -------------------------------------------------------

# C_agg_EIOU_2 <- a$C_mats_EIOU_agg[[1]] |> as.matrix()
# C_agg_Y_2 <- a$C_mats_Y_agg[[1]] |> as.matrix()
# C_agg_EIOU_Y_2 <- a$C_mats_EIOU_Y_agg[[1]] |> as.matrix()
# 
# 
# all(abs(C_agg_EIOU - C_agg_EIOU_2) < 1e-4)
# all(abs(C_agg_Y - C_agg_Y_2) < 1e-4)
# all(abs(C_agg_EIOU_Y - C_agg_EIOU_Y_2) < 1e-4)




# OKAY SO FOR PRODUCTS FOR WHICH THE SUM IS ZERO, THEN THE EFFICIENCY WILL YIELD ZERO
# THESE NEED TO BE FILTERED OUT AS IT MEANS THAT THE PRODUCT IS *NOT* USED ANYWHERE DOMESTICALLY.
# HOW TO FILTER OUT IN THE MATRIX THOUGH??

# Writing test for C_agg_EIOU and C_agg_Y, sum by product should equal 1

# Try within data frame directly:

glimpse(Etafuvecs)

eta_fu_agg <- Cmatsagg |> 
  dplyr::left_join(Etafuvecs, by = c("Country", "Method", "Energy.type", "Last.stage", "Year")) |> 
  dplyr::mutate(
    eta_p_eiou = matsbyname::matrixproduct_byname(C_EIOU_agg, eta.fu) |> 
      matsbyname::clean_byname(margin = 1),
    eta_p_y = matsbyname::matrixproduct_byname(C_Y_agg, eta.fu) |> 
      matsbyname::clean_byname(margin = 1),
    eta_p_eiou_y = matsbyname::matrixproduct_byname(C_EIOU_Y_agg, eta.fu)|> 
      matsbyname::clean_byname(margin = 1)
  ) |> 
  glimpse()


# test <- eta_fu_agg |> 
#   dplyr::mutate(
#     #b = matsbyname::getzerorowcolnames_byname(eta_p_y),
#     eta_p_y_excl_zeros = matsbyname::clean_byname(eta_p_y, margin = 1, clean_value = 0)
#   ) |> 
#   glimpse()



# REMOVING ZERO VALUES HERE
# b <- eta_p_Y |> matsbyname::getzerorowcolnames_byname()
# c <- eta_p_Y |> matsbyname::select_rows_byname(remove_pattern = RCLabels::make_or_pattern(b))



eta_p_EIOU_2 <- eta_fu_agg$eta_p_eiou[[1]] |> as.matrix()
eta_p_Y_2 <- eta_fu_agg$eta_p_y[[1]] |> as.matrix()
eta_p_EIOU_Y_2 <- eta_fu_agg$eta_p_eiou_y[[1]] |> as.matrix()


# Last step: working out aggregated efficiencies

eta_p_EIOU <- matsbyname::matrixproduct_byname(
  C_agg_EIOU,
  eta_i
)

View(eta_p_EIOU)

eta_p_Y <- matsbyname::matrixproduct_byname(
  C_agg_Y,
  eta_i
)

View(eta_p_Y)

eta_p_EIOU_Y <- matsbyname::matrixproduct_byname(
  C_agg_EIOU_Y,
  eta_i
)


all(abs(eta_p_EIOU - eta_p_EIOU_2) < 1e-4)
all(abs(eta_p_Y - eta_p_Y_2) < 1e-4)
all(abs(eta_p_EIOU_Y - eta_p_EIOU_Y_2) < 1e-4)



# REMOVING ZERO VALUES HERE
# b <- eta_p_Y |> matsbyname::getzerorowcolnames_byname()
# c <- eta_p_Y |> matsbyname::select_rows_byname(remove_pattern = RCLabels::make_or_pattern(b))



# Excluding NEU from C_Y matrix

Cmats_excl_NEU <- Cmatsagg |> 
  dplyr::mutate(
    # New C_Y_agg excluding non-energy uses
    C_Y_agg_excl_NEU = matsbyname::matrixproduct_byname(
      matsbyname::select_cols_byname(C_Y_agg, remove_pattern = list("Non-energy consumption -> NEU")) |> 
        matsbyname::rowsums_byname() |> 
        matsbyname::hatinv_byname(), 
      matsbyname::select_cols_byname(C_Y_agg, remove_pattern = list("Non-energy consumption -> NEU"))),
    # New C_EIOU_agg excluding non-energy uses
    C_EIOU_agg_excl_NEU = matsbyname::matrixproduct_byname(
      matsbyname::select_cols_byname(C_EIOU_agg, remove_pattern = list("Non-energy consumption -> NEU")) |>
        matsbyname::rowsums_byname() |>
        matsbyname::hatinv_byname(keep = "rownames"),
      matsbyname::select_cols_byname(C_EIOU_agg, remove_pattern = list("Non-energy consumption -> NEU"))),
    # New C_EIOU_Y_agg excluding non-energy uses
    C_EIOU_Y_agg_excl_NEU = matsbyname::matrixproduct_byname(
      matsbyname::select_cols_byname(C_EIOU_Y_agg, remove_pattern = list("Non-energy consumption -> NEU")) |> 
        matsbyname::rowsums_byname() |> 
        matsbyname::hatinv_byname(), 
      matsbyname::select_cols_byname(C_EIOU_Y_agg, remove_pattern = list("Non-energy consumption -> NEU")))
  ) |> 
  dplyr::select(Country, Method, Energy.type, Last.stage, Year, C_EIOU_agg_excl_NEU, C_Y_agg_excl_NEU, C_EIOU_Y_agg_excl_NEU) |> 
  glimpse()
  
  

C_agg_Y_filtered <- C_agg_Y |> 
  matsbyname::select_cols_byname(remove_pattern = "Non-energy consumption -> NEU")

C_agg_EIOU_filtered <- C_agg_EIOU |> 
  matsbyname::select_cols_byname(remove_pattern = "Non-energy consumption -> NEU")

C_agg_EIOU_Y_filtered <- C_agg_EIOU_Y |> 
  matsbyname::select_cols_byname(remove_pattern = "Non-energy consumption -> NEU")


c_Y <- matsbyname::rowsums_byname(C_agg_Y_filtered)
c_EIOU <- matsbyname::rowsums_byname(C_agg_EIOU_filtered)
c_EIOU_Y <- matsbyname::rowsums_byname(C_agg_EIOU_Y_filtered)


C_agg_Y_excl_neu <- matsbyname::matrixproduct_byname(
  matsbyname::hatinv_byname(c_Y),
  C_agg_Y_filtered
)

C_agg_EIOU_excl_neu <- matsbyname::matrixproduct_byname(
  matsbyname::hatinv_byname(c_EIOU),
  C_agg_EIOU_filtered
)

C_agg_EIOU_Y_excl_neu <- matsbyname::matrixproduct_byname(
  matsbyname::hatinv_byname(c_EIOU_Y),
  C_agg_EIOU_Y_filtered
)


C_agg_Y_excl_neu_2 <- as.matrix(Cmats_excl_NEU$C_Y_agg_excl_NEU[[1]])
C_agg_EIOU_excl_neu_2 <- as.matrix(Cmats_excl_NEU$C_EIOU_agg_excl_NEU[[1]])
C_agg_EIOU_Y_excl_neu_2 <- as.matrix(Cmats_excl_NEU$C_EIOU_Y_agg_excl_NEU[[1]])

all(abs(C_agg_Y_excl_neu_2 - C_agg_Y_excl_neu) < 1e-4)
all(abs(C_agg_EIOU_excl_neu_2 - C_agg_EIOU_excl_neu) < 1e-4)
all(abs(C_agg_EIOU_Y_excl_neu_2 - C_agg_EIOU_Y_excl_neu) < 1e-4)



# Checking final result

eta_p_eiou_inc_NEU_WF <- EtafuYEIOUagg$eta_p_eiou[[1]] |> as.matrix()
eta_p_y_inc_NEU_WF <- EtafuYEIOUagg$eta_p_y[[1]] |> as.matrix()
eta_p_eiou_y_inc_NEU_WF <- EtafuYEIOUagg$eta_p_eiou_y[[1]] |> as.matrix()

eta_p_EIOU_inc_NEU <- eta_p_EIOU |> matsbyname::clean_byname()
eta_p_Y_inc_NEU <- eta_p_Y |> matsbyname::clean_byname()
eta_p_EIOU_Y_inc_NEU <- eta_p_EIOU_Y |> matsbyname::clean_byname()

all(abs(eta_p_EIOU_inc_NEU - eta_p_eiou_inc_NEU_WF) < 1e-4)
all(abs(eta_p_Y_inc_NEU - eta_p_y_inc_NEU_WF) < 1e-4)
all(abs(eta_p_EIOU_Y_inc_NEU - eta_p_eiou_y_inc_NEU_WF) < 1e-4)


# But also checking when excluding non-energy uses...

eta_p_eiou_excl_NEU_WF <- EtafuYEIOUagg$eta_p_eiou[[7]] |> as.matrix()
eta_p_y_excl_NEU_WF <- EtafuYEIOUagg$eta_p_y[[7]] |> as.matrix()
eta_p_eiou_y_excl_NEU_WF <- EtafuYEIOUagg$eta_p_eiou_y[[7]] |> as.matrix()

eta_p_EIOU_excl_NEU <- matsbyname::matrixproduct_byname(C_agg_EIOU_excl_neu, eta_i) |> 
  matsbyname::clean_byname()

eta_p_Y_excl_NEU <- matsbyname::matrixproduct_byname(C_agg_Y_excl_neu, eta_i) |> 
  matsbyname::clean_byname()

eta_p_EIOU_Y_excl_NEU <- matsbyname::matrixproduct_byname(C_agg_EIOU_Y_excl_neu, eta_i) |> 
  matsbyname::clean_byname()

all(abs(eta_p_EIOU_excl_NEU - eta_p_eiou_excl_NEU_WF) < 1e-4)
all(abs(eta_p_Y_excl_NEU - eta_p_y_excl_NEU_WF) < 1e-4)
all(abs(eta_p_EIOU_Y_excl_NEU - eta_p_eiou_y_excl_NEU_WF) < 1e-4)






# And sum of C matrices - Looking good.

glimpse(EtafuYEIOUagg)

View(EtafuYEIOUagg$C_EIOU_agg[[7]] |> matsbyname::rowsums_byname() |> as.matrix())
View(EtafuYEIOUagg$C_Y_agg[[7]] |> matsbyname::rowsums_byname() |> as.matrix())
View(EtafuYEIOUagg$C_EIOU_Y_agg[[7]] |> matsbyname::rowsums_byname() |> as.matrix())





# Testing Exiobase coeffs progress ----------------------------------------

tar_load(EtafuYEIOUagg)

glimpse(EtafuYEIOUagg)


calc_Ef_to_Eu_exiobase(EtafuYEIOU_mats = EtafuYEIOU,
                       eta_fu_Y_EIOU_agg = EtafuYEIOUagg,
                       years_exiobase = ExiobaseYears,
                       full_list_exiobase_flows = ListExiobaseEnergyFlows,
                       country_concordance_table_df = CountryConcordanceTable)


eta_fu_economy_wide_df <- EtafuYEIOUagg |> 
  dplyr::filter(Energy.type == "E", NEU == "Excluded") |> 
  dplyr::select(Country, Method, Energy.type, Last.stage, Year, eta_p_eiou_y) |> 
  tidyr::pivot_longer(cols = eta_p_eiou_y, values_to = "matvals", names_to = "matnames") |> 
  matsindf::expand_to_tidy() |> 
  dplyr::rename(
    Product = rownames,
    PFU.code = Country,
    eta = "matvals"
  ) |> 
  dplyr::select(PFU.code, Method, Energy.type, Last.stage, Year, Product, eta) |>
  dplyr::mutate(Exiobase.Flow = "Economy-wide") |> 
  print()








# Drafting Eta agg function ---------------------------------------------

# Trying to draft the function

tar_load(CmatsAgg)
tar_load(Etafuvecs)
tar_load(Phivecs)

calc_fu_Y_EIOU_agg_efficiencies(C_mats_agg = CmatsAgg, 
                                eta_fu_vecs = Etafuvecs,
                                phi_vecs = Phivecs)



# (2) Calculation of efficiencies excluding non-energy uses
# (1) Determination of aggregated C_mats excluding non-energy uses
C_mats_agg_excl_NEU <- C_mats_agg |> 
  dplyr::mutate(
    # New C_Y_agg excluding non-energy uses
    "{C_Y_agg_excl_NEU}" := matsbyname::matrixproduct_byname(
      matsbyname::select_cols_byname(.data[[C_Y_agg]], remove_pattern = list(non_energy_use_machine)) |> 
        matsbyname::rowsums_byname() |> 
        matsbyname::hatinv_byname(), 
      matsbyname::select_cols_byname(.data[[C_Y_agg]], remove_pattern = list(non_energy_use_machine))),
    # New C_EIOU_agg excluding non-energy uses
    "{C_EIOU_agg_excl_NEU}" := matsbyname::matrixproduct_byname(
      matsbyname::select_cols_byname(.data[[C_EIOU_agg]], remove_pattern = list(non_energy_use_machine)) |>
        matsbyname::rowsums_byname() |>
        matsbyname::hatinv_byname(keep = "rownames"),
      matsbyname::select_cols_byname(.data[[C_EIOU_agg]], remove_pattern = list(non_energy_use_machine))),
    # New C_EIOU_Y_agg excluding non-energy uses
    "{C_EIOU_Y_agg_excl_NEU}" := matsbyname::matrixproduct_byname(
      matsbyname::select_cols_byname(.data[[C_EIOU_Y_agg]], remove_pattern = list(non_energy_use_machine)) |> 
        matsbyname::rowsums_byname() |> 
        matsbyname::hatinv_byname(), 
      matsbyname::select_cols_byname(.data[[C_EIOU_Y_agg]], remove_pattern = list(non_energy_use_machine)))
  ) |> 
  dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[C_EIOU_agg_excl_NEU]],
                .data[[C_Y_agg_excl_NEU]], .data[[C_EIOU_Y_agg_excl_NEU]]) |> 
  dplyr::rename(
    "{C_EIOU_agg}" := .data[[C_EIOU_agg_excl_NEU]],
    "{C_Y_agg}" := .data[[C_Y_agg_excl_NEU]],
    "{C_EIOU_Y_agg}" := .data[[C_EIOU_Y_agg_excl_NEU]]
  )

# (2) Determination of aggregated efficiencies (excluding non-energy uses)
eta_fu_agg <- C_mats_agg_excl_NEU |> 
  dplyr::left_join(eta_fu_vecs, by = c({country}, {method}, {energy_type}, {last_stage}, {year})) |> 
  dplyr::mutate(
    "{eta_p_eiou}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_agg]], .data[[eta.fu]]) |> 
      matsbyname::clean_byname(margin = 1),
    "{eta_p_y}" := matsbyname::matrixproduct_byname(.data[[C_Y_agg]], .data[[eta.fu]]) |> 
      matsbyname::clean_byname(margin = 1),
    "{eta_p_eiou_y}" := matsbyname::matrixproduct_byname(.data[[C_EIOU_Y_agg]], .data[[eta.fu]]) |> 
      matsbyname::clean_byname(margin = 1)#,
    #"{neu}" := excluded
  )



tar_load(EtafuYEIOU)
tar_load(EtafuYEIOUagg)
tar_load(ExiobaseYears)
tar_load(ListExiobaseEnergyFlows)
tar_load(CountryConcordanceTable)

calc_Ef_to_Eu_exiobase(eta_fu_Y_EIOU_mats = EtafuYEIOU,
                       eta_fu_Y_EIOU_agg = EtafuYEIOUagg,
                       years_exiobase = ExiobaseYears,
                       full_list_exiobase_flows = ListExiobaseEnergyFlows,
                       country_concordance_table_df = CountryConcordanceTable)

tar_load(Phivecs)
tar_load(EtafuPhiYEIOUagg)

calc_Ef_to_Xu_exiobase(EtafuYEIOU_mats = EtafuYEIOU,
                       phi_vecs = Phivecs,
                       eta_fu_phi_Y_EIOU_agg = EtafuPhiYEIOUagg,
                       years_exiobase = ExiobaseYears,
                       full_list_exiobase_flows = ListExiobaseEnergyFlows,
                       country_concordance_table_df = CountryConcordanceTable)

# Filtering out non useful energy flows
# So here we remove non-energy uses and losses
list_useful_energy_flows <- full_list_exiobase_flows %>%
  dplyr::filter(Useful.energy.flow == TRUE) %>%
  dplyr::select(Exiobase.Flow, PFU.flow)

# Expanding the EIOU-wide efficiencies*phi values data frame to prepare the join
eta_times_phi_EIOU_wide_df <- eta_fu_phi_Y_EIOU_agg |> 
  dplyr::select(Country, Method, Year, eta_phi_p_eiou) |> 
  tidyr::pivot_longer(cols = eta_phi_p_eiou, values_to = "matvals", names_to = "matnames") |> 
  matsindf::expand_to_tidy() |> 
  dplyr::rename(
    Product = rownames,
    phi_eta_X = "matvals"
  ) |> 
  dplyr::select(Country, Method, Year, Product, phi_eta_X) |>
  dplyr::mutate(PFU.flow = "EIOU-wide",
                Last.stage = "Final",
                Energy.type = "E")

# Expanding Phivecs
phi_vals_df <- phi_vecs |> 
  dplyr::filter(Year %in% years_exiobase) |> 
  tidyr::pivot_longer(cols = phi, names_to = "matnames", values_to = "matvals") |> 
  matsindf::expand_to_tidy(rownames = "Product") |> 
  dplyr::select(-matnames, -colnames, -rowtypes, -coltypes) |> 
  dplyr::rename(PFU.code = Country,
                phi = matvals)

# Expanding the final-to-useful efficiencies to prepare the join
phi_eta_fu_df <- EtafuYEIOU_mats |>
  dplyr::filter(Year %in% years_exiobase) |> 
  dplyr::select(Country:Year, eta_fu_Y_X:eta_fu_EIOU_X) |> 
  tidyr::pivot_longer(cols = tidyr::ends_with("_X"), names_to = "matnames", values_to = "matvals") |> 
  matsindf::expand_to_tidy(rownames = "Product", colnames = "PFU.flow") |> 
  dplyr::select(-rowtypes, -coltypes, -matnames) |> 
  dplyr::rename(eta = matvals) |> 
  dplyr::filter(Product %in% IEATools::products) |>
  dplyr::filter(eta != 0) |> 
  dplyr::left_join(phi_vals_df |> dplyr::rename(Country = PFU.code), by = c("Country", "Year", "Product")) |> 
  dplyr::mutate(
    phi_eta_X = phi * eta
  ) |> 
  dplyr::select(-eta, -phi) |> 
  dplyr::bind_rows(eta_times_phi_EIOU_wide_df)

# Expanding the economy-wide efficiencies*phi values data frame to prepare the join
eta_times_phi_economy_wide_df <- eta_fu_phi_Y_EIOU_agg |> 
  dplyr::select(Country, Method, Year, eta_phi_p_eiou_y) |> 
  tidyr::pivot_longer(cols = eta_phi_p_eiou_y, values_to = "matvals", names_to = "matnames") |> 
  matsindf::expand_to_tidy() |> 
  dplyr::rename(
    Product = rownames,
    PFU.code = Country,
    phi_eta_X = "matvals"
  ) |> 
  dplyr::select(PFU.code, Method, Year, Product, phi_eta_X) |>
  dplyr::mutate(Exiobase.Flow = "Economy-wide")

# Preparing the (Country, Year, Product) list
country_year_product_list <- phi_eta_fu_df |> 
  dplyr::select(Country, Year, Product) |> 
  dplyr::distinct()

# List of Exiobase flows to be joined to the PFU results
expanded_exiobase_Ue_flows <- list_useful_energy_flows |> 
  tidyr::expand_grid(country_year_product_list) |> 
  dplyr::relocate(Exiobase.Flow:PFU.flow, .after = Year)

# Now, joining to ascribe each Exiobase flow a PFU flow
Ef_to_Xu_multipliers <- expanded_exiobase_Ue_flows |> 
  dplyr::left_join(phi_eta_fu_df, by = c("Country", "Year", "Product", "PFU.flow")) |> 
  dplyr::filter(!is.na(phi_eta_X)) |> 
  dplyr::rename(PFU.code = Country) |> 
  dplyr::select(-PFU.flow) |> 
  dplyr::bind_rows(eta_times_phi_economy_wide_df) |> 
  dplyr::left_join(country_concordance_table_df %>% dplyr::select(IEA.name.accented, PFU.code),
                   by = "PFU.code") |>
  dplyr::rename(IEA.country.name = IEA.name.accented,
                Flow = Exiobase.Flow) |>
  dplyr::filter(!is.na(IEA.country.name)) |>
  dplyr::select(IEA.country.name, Year, Product, Flow, phi_eta_X) |>
  dplyr::relocate(Flow, .before = Product) |>
  tidyr::pivot_wider(names_from = Year, values_from = phi_eta_X)



