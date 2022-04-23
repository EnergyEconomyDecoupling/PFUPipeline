#' Create a data frame of phi_pf vectors
#'
#' This function creates a data frame that contains all the metadata columns
#' from `phi_u_vecs` and a column of phi_pf vectors.
#' This work is accomplished by creating a vector from `phi_constants`,
#' adding one instance of the vector to the right side of the `phi_constants` data frame
#' for each row of the data frame,
#' and deleting `phi_u_colname` from the data frame.
#'
#' @param phi_constants A data frame of constant phi values (for primary, final, and useful stages)
#'                      with columns `product`, `phi_colname`, and `is_useful_colname`.
#' @param phi_u_vecs A data frame containing metadata columns and a column of phi_u vectors.
#'                   A column of phi_pf vectors replaces the column of phi_u vectors on output.
#' @param countries The countries for which you want to perform this task.
#' @param country,product See `IEATools::iea_cols`.
#' @param eta_fu,phi_u,phi_pf_colname See `IEATools::template_cols`.
#' @param phi_colname,is_useful_colname See `IEATools::phi_constants_colnames`.
#'
#' @return A version of the `phi_constants` data frame
#'         with the column of useful phi (useful exergy-to-energy ratio) vectors
#'         replaced by a column of primary and final phi vectors.
#'
#' @export
#'
#' @examples
#' phi_constants <- IEATools::sample_phi_constants_path() %>%
#'   IEATools::load_phi_constants_table()
#' phi_u_vecs <- tibble::tibble(Country = "GHA",
#'                              Year = 1971,
#'                              rownames = c("Light", "MD"),
#'                              colnames = "col",
#'                              matnames = "phi.u",
#'                              matvals = c(0.8, 0.9),
#'                              rowtypes = "rowtype",
#'                              coltypes = "coltype") %>%
#'   dplyr::group_by(Country, Year) %>%
#'   matsindf::collapse_to_matrices() %>%
#'   dplyr::rename(phi.u = matvals) %>%
#'   dplyr::mutate(
#'     Quantity = NULL
#'   )
#' calc_phi_pf_vecs(phi_constants, phi_u_vecs, countries = "GHA")
calc_phi_pf_vecs <- function(phi_constants,
                             phi_u_vecs,
                             countries,
                             country = IEATools::iea_cols$country,
                             product = IEATools::iea_cols$product,
                             # quantity = IEATools::template_cols$quantity,
                             eta_fu = IEATools::template_cols$eta_fu,
                             phi_u = IEATools::template_cols$phi_u,
                             phi_pf_colname = IEATools::template_cols$phi_pf,
                             phi_colname = IEATools::phi_constants_names$phi_colname,
                             is_useful_colname = IEATools::phi_constants_names$is_useful_colname) {
  # Pick up non-useful (i.e., primary and final)
  # phi values.
  phi_pf_constants <- phi_constants %>%
    dplyr::filter(! .data[[is_useful_colname]])
  # Create a vector from phi_pf_constants
  phi_pf_vec <- matrix(phi_pf_constants[[phi_colname]], nrow = nrow(phi_pf_constants), ncol = 1,
                       dimnames = list(c(phi_pf_constants[[product]]), phi_colname)) %>%
    matsbyname::setrowtype(product) %>% matsbyname::setcoltype(phi_colname)
  
  trimmed_phi_u_vecs <- phi_u_vecs %>%
    dplyr::filter(.data[[country]] %in% countries) %>%
    dplyr::mutate(
      # We don't need the eta_fu or phi_u column on output.
      "{eta_fu}" := NULL,
      "{phi_u}" := NULL
    )
  nrows_trimmed_phi_u_vecs <- nrow(trimmed_phi_u_vecs)
  
  trimmed_phi_u_vecs %>%
    dplyr::mutate(
      # Add a column of phi_pf vectors
      "{phi_pf_colname}" := matsbyname::make_list(phi_pf_vec,
                                                  n = nrows_trimmed_phi_u_vecs,
                                                  lenx = 1)
    )
}


#' Sums phi_pf and phi_u vectors
#'
#' This function verifies that there are no rows in common between the
#' two input vectors.
#'
#' @param phi_pf_vecs A data frame of phi_pf vectors.
#' @param phi_u_vecs A data frame of phi_u vectors.
#' @param countries The countries for which you want to perform this task.
#' @param country,last_stage,energy_type,method See `IEATools::iea_cols`.
#' @param phi_pf_colname,phi_u_colname See `IEATools::template_cols`.
#' @param phi_colname See `IEATools::phi_constants_names`.
#' @param .nrow_diffs,.phi_shape_OK,.phi_names_OK,.phi_cols_OK,.phi_sum_OK,.phi_pf_colnames,.phi_u_colnames Names of temporary error-checking columns created internally.
#'
#' @return A data frame of summed phi_pf and phi_u vectors.
#'
#' @export
#'
#' @examples
#' phi_pf_vec <- matrix(c(1.1,
#'                        1.05), nrow = 2, ncol = 1,
#'                      dimnames = list(c("Coal", "Oil"), "phi"))
#' # Make a data frame of phi_pf vectors
#' phi_pf <- tibble::tibble(phi.pf = matsbyname::make_list(phi_pf_vec, n = 2, lenx = 1),
#'                          Country = "GHA",
#'                          Year = c(1971, 2000))
#' phi_u_vec <- matrix(c(0.8,
#'                       0.9,
#'                       0.7), nrow = 3, ncol = 1,
#'                     dimnames = list(c("Light", "MD", "Propulsion"), "phi"))
#' phi_u <- tibble::tibble(phi.u = matsbyname::make_list(phi_u_vec, n = 2, lenx = 1),
#'                         Country = "GHA",
#'                         Year = c(1971, 2000))
#' sum_phi_vecs(phi_pf, phi_u, countries = "GHA")
sum_phi_vecs <- function(phi_pf_vecs,
                         phi_u_vecs,
                         countries,
                         country = IEATools::iea_cols$country,
                         last_stage = IEATools::iea_cols$last_stage,
                         energy_type = IEATools::iea_cols$energy_type,
                         method = IEATools::iea_cols$method,
                         phi_pf_colname = IEATools::template_cols$phi_pf,
                         phi_u_colname = IEATools::template_cols$phi_u,
                         phi_colname = IEATools::phi_constants_names$phi_colname,
                         .nrow_diffs = ".nrow_diffs",
                         .phi_shape_OK = ".phi_shape_OK",
                         .phi_names_OK = ".phi_names_OK",
                         .phi_cols_OK = ".phi_cols_OK",
                         .phi_sum_OK = ".phi_sum_OK",
                         .phi_pf_colnames = ".phi_pf_colnames",
                         .phi_u_colnames = ".phi_u_colnames") {
  
  phi_df <- dplyr::full_join(phi_pf_vecs,
                             phi_u_vecs,
                             by = matsindf::everything_except(phi_pf_vecs, phi_pf_colname) %>% as.character()) %>%
    dplyr::filter(.data[[country]] %in% countries) %>%
    dplyr::mutate(
      # Check that all phi vectors have 1 column.
      "{.phi_shape_OK}" := (matsbyname::ncol_byname(.data[[phi_pf_colname]]) == 1) &
        (matsbyname::ncol_byname(.data[[phi_u_colname]]) == 1)
    )
  if (! all(phi_df[[.phi_shape_OK]])) {
    # Prepare an error message.
    bad_rows <- phi_df %>%
      dplyr::filter(!.data[[.phi_shape_OK]])
    err_msg <- paste("In sum_phi_vecs(), need phi vectors with one column only. These vectors are bad:", matsindf::df_to_msg(bad_rows))
    stop(err_msg)
  }
  
  out <- phi_df %>%
    dplyr::mutate(
      "{phi_colname}" := matsbyname::sum_byname(.data[[phi_pf_colname]], .data[[phi_u_colname]]),
      "{.phi_shape_OK}" := NULL
    )
  
  # Check that the length of each phi vector is the sum of the lengths of the phi_pf and phi_u vectors.
  # If not, there are duplicate rows in the vectors, which should be an error.
  # There should be no primary-final energy carriers that are also useful energy carriers.
  # Also check that the result of the sum is a single column.
  # If we get 2 or more columns, it means that the column names were different for phi_pf and phi_u,
  # which is an error.
  
  err_check <- out %>%
    dplyr::mutate(
      "{.nrow_diffs}" := matsbyname::nrow_byname(.data[[phi_pf_colname]]) %>% as.numeric() +
        matsbyname::nrow_byname(.data[[phi_u_colname]]) %>% as.numeric() -
        matsbyname::nrow_byname(.data[[phi_colname]]) %>% as.numeric(),
      "{.phi_sum_OK}" := matsbyname::iszero_byname(.data[[.nrow_diffs]]),
      "{.phi_cols_OK}" := matsbyname::ncol_byname(.data[[phi_colname]]) == 1
    )
  
  if (!all(err_check[[.phi_sum_OK]])) {
    # There is a problem.
    problem_rows <- err_check %>%
      dplyr::filter(!.data[[.phi_sum_OK]]) %>%
      dplyr::mutate(
        "{.nrow_diffs}" := NULL,
        "{.phi_sum_OK}" := NULL,
        "{.phi_cols_OK}" := NULL,
        "{phi_pf_colname}" := NULL,
        "{phi_u_colname}" := NULL,
        "{phi_colname}" := NULL
      )
    
    err_msg <- paste("In PFUWorkflow::sum_phi_vecs(), the length of the sum of phi_pf and phi_u vectors",
                     "was not the same as the sum of vector lengths. The rows that failed the test are",
                     matsindf::df_to_msg(problem_rows))
    stop(err_msg)
  }
  if (!all(err_check[[.phi_cols_OK]])) {
    # There is a problem.
    problem_rows <- err_check %>%
      dplyr::filter(!.data[[.phi_cols_OK]]) %>%
      dplyr::mutate(
        "{phi_pf_colname}" := paste(phi_pf_colname, "=", matsbyname::getcolnames_byname(.data[[phi_pf_colname]])),
        "{phi_u_colname}" := paste(phi_u_colname, "=", matsbyname::getcolnames_byname(.data[[phi_u_colname]])),
        "{.nrow_diffs}" := NULL,
        "{.phi_sum_OK}" := NULL,
        "{.phi_cols_OK}" := NULL,
        "{phi_colname}" := NULL
      )
    err_msg <- paste("In PFUWorkflow::sum_phi_vecs(), the names of the phi.pf and phi.u columns should be the same.",
                     "Rows that failed the test are",
                     matsindf::df_to_msg(problem_rows))
    stop(err_msg)
  }
  
  out %>%
    dplyr::mutate(
      # Delete the columns we no longer need.
      # These are not relevant
      # The output works for energy at all stages of the energy conversion chain,
      # so we don't need to track last stage.
      "{last_stage}" := NULL,
      # This function converts from energy to exergy, so we
      # should remove dependence on energy type.
      "{energy_type}" := NULL,
      # This function work for any method of counting the primary energy of renewable electricity.
      # Once we have the primary energy of renewable electricity,
      # this function will have identified the exergy-to-energy ratio associated with that
      # primary energy carrier.
      "{method}" := NULL,
      # These were temporary columns
      "{phi_pf_colname}" := NULL,
      "{phi_u_colname}" := NULL
    )
}


#' Move from all exergy quantities to all energy quantities in energy conversion chains
#'
#' Converts energy conversion chains represented by the matrices
#' in the data frame of `psut_energy` from energy quantities to exergy quantities.
#'
#' The steps in this calculation are to join phi_vecs to psut_energy.
#' Thereafter, we call into the `IEATools` package to do the matrix multiplications.
#'
#' @param psut_energy A wide-by-matrices data frame of energy conversion chain data.
#' @param phi_vecs A data frame of vectors of phi (exergy-to-energy ratios)
#' @param countries The countries for which this task should be performed.
#' @param country See `IEATools::iea_cols`.
#' @param phi_colname See `IEATools::phi_constants`.
#'
#' @return A version of `psut_energy` with additional rows
#'
#' @export
move_to_exergy <- function(psut_energy,
                           phi_vecs,
                           countries,
                           country = IEATools::iea_cols$country,
                           phi_colname = IEATools::phi_constants_names$phi_colname) {
  # Make sure we're operating on the countries of interest.
  psut_energy <- psut_energy %>%
    dplyr::filter(.data[[country]] %in% countries)
  phi_vecs <- phi_vecs %>%
    dplyr::filter(.data[[country]] %in% countries)
  
  # Get the metadata columns for the phi_vecs data frame.
  meta_cols <- matsindf::everything_except(phi_vecs, phi_colname, .symbols = FALSE)
  
  # Join the phi vectors to the psut_energy data frame
  df_with_phi <- dplyr::left_join(psut_energy, phi_vecs, by = meta_cols)
  
  # Calculate exergy versions of the ECC.
  # Need to specify the mat_piece here, because the default value ("all")
  # is not appropriate.
  # We will have cases where the matrix will have specified names like
  # "MP [from Bulk carrier ships]".
  # In this case, we need to match the noun, not the whole string.
  Recca::extend_to_exergy(df_with_phi, mat_piece = "noun", phi_piece = "all")
}