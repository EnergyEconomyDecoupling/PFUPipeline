#' Load FU allocation tables
#'
#' This function reads all final-to-useful allocation data
#' in files in the `fu_analysis_folder` that start with the country abbreviations
#' given in `countries`.
#'
#' By default, it is assumed that each country's final-to-useful analysis file will be in a subfolder
#' of `fu_analysis_path`.
#' Set `use_subfolders` to `FALSE` to change the default behavior.
#'
#' If final-to-useful allocation data are not available, this function
#' automatically creates an empty final-to-useful allocation template and writes it to disk.
#' Then, this function reads the empty file.
#' This behavior can be modified by setting argument `generate_missing_fu_allocation_template` to `FALSE`.
#'
#' @param fu_analysis_folder The folder from which final-to-useful analyses will be loaded.
#' @param specified_iea_data A data frame of specified IEA data for `countries`.
#' @param countries The countries for which allocation tables should be loaded.
#' @param file_suffix The suffix for the FU analysis files. Default is "`r IEATools::fu_analysis_file_info$fu_analysis_file_suffix`".
#' @param use_subfolders Tells whether to look for files in subfolders named by `countries`. Default is `TRUE`.
#' @param generate_missing_fu_allocation_template Tells whether to generate a missing final-to-useful allocation template from `specified_iea_data`. Default is `TRUE`.
#' @param fu_allocations_tab_name The name of the tab for final-to-useful allocations in the Excel file containing final-to-useful allocation data. Default is "`r IEATools::fu_analysis_file_info$fu_allocation_tab_name`".
#'
#' @export
#'
#' @return A data frame of FU Allocation tables read by `IEATools::load_fu_allocation_data()`.
#'         If no FU Allocation data are found and `generate_missing_fu_allocation_template` is `TRUE`,
#'         an empty template written to disk and the empty template is returned.
#'         If no FU Allocation data are found and `generate_missing_fu_allocation_template` is `FALSE`,
#'         `NULL` is returned.
load_fu_allocation_tables <- function(fu_analysis_folder,
                                      specified_iea_data,
                                      countries,
                                      file_suffix = IEATools::fu_analysis_file_info$fu_analysis_file_suffix,
                                      use_subfolders = TRUE,
                                      generate_missing_fu_allocation_template = TRUE,
                                      fu_allocations_tab_name = IEATools::fu_analysis_file_info$fu_allocation_tab_name) {
  out <- lapply(countries, FUN = function(coun) {
    folder <- ifelse(use_subfolders, file.path(fu_analysis_folder, coun), fu_analysis_folder)
    fpath <- file.path(folder, paste0(coun, file_suffix))
    fexists <- file.exists(fpath)
    if (!fexists & !generate_missing_fu_allocation_template) {
      return(NULL)
    }
    if (!fexists & generate_missing_fu_allocation_template) {
      # Make sure we have the folder we need
      dir.create(folder, showWarnings = FALSE)
      # Create and write the template
      iea_data <- specified_iea_data %>%
        dplyr::filter(.data[[IEATools::iea_cols$country]] == coun)
      IEATools::fu_allocation_template(iea_data) %>%
        IEATools::write_fu_allocation_template(fpath)
    }
    # Read the FU allocation data from fpath.
    IEATools::load_fu_allocation_data(fpath, fu_allocations_tab_name = fu_allocations_tab_name)
  }) %>%
    dplyr::bind_rows()
  if (nrow(out) == 0) {
    return(NULL)
  }
  return(out)
}


#' Assemble completed final-to-useful allocation tables
#'
#' This function is used in a drake workflow to assemble completed final-to-useful allocation tables
#' given a set of incomplete allocation tables.
#'
#' Note that this function can accept tidy or wide by year data frames.
#' The return value is a tidy data frame.
#' Information from exemplar countries is used to complete incomplete final-to-useful efficiency tables.
#' See examples for how to construct `exemplar_lists`.
#'
#' @param incomplete_allocation_tables A data frame containing (potentially) incomplete final-to-useful allocation tables.
#'                                     This data frame may be tidy or wide by years.
#' @param exemplar_lists A data frame containing `country` and `year` columns along with a column of ordered vectors of strings
#'                       telling which countries should be considered exemplars for the country and year of this row.
#' @param specified_iea_data A data frame containing specified IEA data.
#' @param countries A vector of countries for which completed final-to-useful allocation tables are to be assembled.
#' @param years The years for which analysis is desired. Default is `NULL`, meaning analyze all years.
#' @param country,year See `IEATools::iea_cols`.
#' @param exemplars,exemplar_tables,iea_data,incomplete_alloc_tables,complete_alloc_tables See `PFUWorkflow::exemplar_names`.
#'
#' @return A tidy data frame containing completed final-to-useful allocation tables.
#'
#' @export
#'
#' @examples
#' # Load final-to-useful allocation tables, but eliminate one category of consumption,
#' # Residential consumption of Primary solid biofuels,
#' # which will be filled by the exemplar for GHA, ZAF.
#' incomplete_fu_allocation_tables <- IEATools::load_fu_allocation_data() %>%
#'   dplyr::filter(! (Country == "GHA" & Ef.product == "Primary solid biofuels" &
#'     Destination == "Residential"))
#' # Show that those rows are gone.
#' incomplete_fu_allocation_tables %>%
#'   dplyr::filter(Country == "GHA" & Ef.product == "Primary solid biofuels" &
#'     Destination == "Residential")
#' # But the missing rows of GHA are present in allocation data for ZAF.
#' incomplete_fu_allocation_tables %>%
#'   dplyr::filter(Country == "ZAF" & Ef.product == "Primary solid biofuels" &
#'     Destination == "Residential")
#' # Set up exemplar list
#' el <- tibble::tribble(
#'   ~Country, ~Year, ~Exemplars,
#'   "GHA", 1971, c("ZAF"),
#'   "GHA", 2000, c("ZAF"))
#' el
#' # Load IEA data
#' iea_data <- IEATools::load_tidy_iea_df() %>%
#'   IEATools::specify_all()
#' # Assemble complete allocation tables
#' completed <- assemble_fu_allocation_tables(incomplete_allocation_tables =
#'                                              incomplete_fu_allocation_tables,
#'                                            exemplar_lists = el,
#'                                            specified_iea_data = iea_data,
#'                                            countries = "GHA")
#' # Missing data for GHA has been picked up from ZAF.
#' completed %>%
#'   dplyr::filter(Country == "GHA" & Ef.product == "Primary solid biofuels" &
#'     Destination == "Residential")
assemble_fu_allocation_tables <- function(incomplete_allocation_tables,
                                          exemplar_lists,
                                          specified_iea_data,
                                          countries,
                                          years = NULL,
                                          country = IEATools::iea_cols$country,
                                          year = IEATools::iea_cols$year,
                                          exemplars = PFUWorkflow::exemplar_names$exemplars,
                                          exemplar_tables = PFUWorkflow::exemplar_names$exemplar_tables,
                                          iea_data = PFUWorkflow::exemplar_names$iea_data,
                                          incomplete_alloc_tables = PFUWorkflow::exemplar_names$incomplete_alloc_table,
                                          complete_alloc_tables = PFUWorkflow::exemplar_names$complete_alloc_table) {
  
  # The incomplete tables are easier to deal with when they are tidy.
  tidy_incomplete_allocation_tables <- IEATools::tidy_fu_allocation_table(incomplete_allocation_tables)
  if (!is.null(years)) {
    max_year <- max(years)
    tidy_incomplete_allocation_tables <- tidy_incomplete_allocation_tables %>%
      dplyr::filter(.data[[year]] <= max_year)
  }
  
  completed_tables_by_year <- lapply(countries, FUN = function(coun) {
    coun_exemplar_strings <- exemplar_lists %>%
      dplyr::filter(.data[[country]] == coun)
    
    # For each combination of Country and Year (the rows of coun_exemplar_strings),
    # assemble a list of country allocation tables.
    coun_exemplar_strings_and_tables <- coun_exemplar_strings %>%
      dplyr::mutate(
        # Create a list column containing lists of exemplar tables
        # corresponding to the countries in the Exemplars column.
        "{exemplar_tables}" := Map(get_one_exemplar_table_list,
                                   # Need to wrap this in a list so the WHOLE table is sent via Map to get_one_exemplar_table_list
                                   tidy_incomplete_tables = list(tidy_incomplete_allocation_tables),
                                   exemplar_strings = .data[[exemplars]],
                                   yr = .data[[year]],
                                   country_colname = country,
                                   year_colname = year),
        # Add a column containing an IEA data frame for the country and year of each row
        "{iea_data}" := Map(get_one_df_by_coun_and_yr,
                            .df = list(specified_iea_data),
                            coun = .data[[country]],
                            yr = .data[[year]],
                            country_colname = country,
                            year_colname = year),
        # Add a column containing incomplete fu allocation tables for each row (i.e., for each combination of country and year).
        "{incomplete_alloc_tables}" := Map(get_one_df_by_coun_and_yr,
                                           .df = list(tidy_incomplete_allocation_tables),
                                           coun = .data[[country]],
                                           yr = .data[[year]],
                                           country_colname = country,
                                           year_colname = year),
        # Add a column containing completed fu allocation tables for each row (i.e., for each combination of country and year).
        # Note that the data frames in this column contain the SOURCE of information for each allocation.
        "{complete_alloc_tables}" := Map(IEATools::complete_fu_allocation_table,
                                         fu_allocation_table = .data[[incomplete_alloc_tables]],
                                         country_to_complete = coun,
                                         exemplar_fu_allocation_tables = .data[[exemplar_tables]],
                                         tidy_specified_iea_data = .data[[iea_data]])
      )
  }) %>%
    dplyr::bind_rows()
  
  # The only information we need to return is the completed allocation tables.
  # Expand (unnest) only the completed allocation table column to give one data frame of all the FU allocations
  # for all years and all countries.
  completed_tables_by_year %>%
    dplyr::select(complete_alloc_tables) %>%
    tidyr::unnest(cols = .data[[complete_alloc_tables]])
}


get_one_df_by_coun_and_yr <- function(.df, coun, yr, country_colname, year_colname) {
  .df %>%
    dplyr::filter(.data[[country_colname]] %in% coun, .data[[year_colname]] %in% yr)
}


get_one_exemplar_table_list <- function(tidy_incomplete_tables,
                                        exemplar_strings, yr, country_colname, year_colname) {
  lapply(exemplar_strings, function(exemplar_coun) {
    tidy_incomplete_tables %>%
      dplyr::filter(.data[[country_colname]] == exemplar_coun, .data[[year_colname]] == yr)
  })
}


#' Assemble completed final-to-useful efficiency tables
#'
#' This function is used in a drake workflow to assemble completed final-to-useful efficiency tables
#' given a set of incomplete efficiency tables.
#' Information from exemplar countries is used to complete incomplete final-to-useful efficiency tables.
#' See examples for how to construct `exemplar_lists`.
#'
#' Note that this function can accept tidy or wide by year data frames.
#' The return value is a tidy data frame.
#'
#' Note that the `.values` argument applies for both
#' `incomplete_eta_fu_tables` and
#' `completed_fu_allocation_tables`.
#' Callers should ensure that value columns in both
#' data frames (`incomplete_eta_fu_tables` and `completed_fu_allocation_tables`)
#' are named identically and that name is passed into the
#' `.values` argument.
#'
#' Note that the `which_quantity` argument is an accident of history.
#' At one time, this function also assembled tables
#' of `phi.u` (useful exergy-to-energy ratio) values.
#' At present, the function only assembles `eta.fu` (final-to-useful efficiency) tables,
#' so the only valid value for `which_quantity` is `IEATools::template_cols$eta_fu`.
#'
#'
#' @param incomplete_eta_fu_tables An incomplete data frame of final-to-useful efficiencies for all Machines in `completed_fu_allocation_tables`.
#' @param exemplar_lists A data frame containing `country` and `year` columns along with a column of ordered vectors of strings
#'                       telling which countries should be considered exemplars for the country and year of this row.
#' @param completed_fu_allocation_tables A data frame containing completed final-to-useful allocation data,
#'                                       typically the result of calling `assemble_fu_allocation_tables`.
#' @param countries A vector of countries for which completed final-to-useful allocation tables are to be assembled.
#' @param years The years for which analysis is desired. Default is `NULL`, meaning analyze all years.
#' @param which_quantity A vector of quantities to be completed in the eta_FU table.
#'                       Default is `c(IEATools::template_cols$eta_fu, IEATools::template_cols$phi_u)`.
#'                       Must be one or both of the default values.
#' @param country,method,energy_type,last_stage,year,unit,e_dot See `IEATools::iea_cols`.
#' @param machine,eu_product,eta_fu,phi_u,c_source,eta_fu_source,e_dot_machine,e_dot_machine_perc,quantity,maximum_values,e_dot_perc,.values See `IEATools::template_cols`.
#' @param exemplars,exemplar_tables,alloc_data,incomplete_eta_tables,complete_eta_tables See `PFUWorkflow::exemplar_names`.
#'
#' @return A tidy data frame containing completed final-to-useful efficiency tables.
#'
#' @export
#'
#' @examples
#' # Make some incomplete efficiency tables for GHA by removing Wood cookstoves.
#' # Information from the exemplar, ZAF, will supply efficiency for Wood cookstoves for GHA.
#' incomplete_eta_fu_tables <- IEATools::load_eta_fu_data() %>%
#'   dplyr::filter(! (Country == "GHA" & Machine == "Wood cookstoves"))
#' # The rows for Wood cookstoves are missing.
#' incomplete_eta_fu_tables %>%
#'   dplyr::filter(Country == "GHA", Machine == "Wood cookstoves")
#' # Set up exemplar list
#' el <- tibble::tribble(
#'   ~Country, ~Year, ~Exemplars,
#'   "GHA", 1971, c("ZAF"),
#'   "GHA", 2000, c("ZAF"))
#' # Load FU allocation data.
#' # An efficiency is needed for each machine in FU allocation data.
#' fu_allocation_data <- IEATools::load_fu_allocation_data()
#' # Assemble complete allocation tables
#' completed <- assemble_eta_fu_tables(incomplete_eta_fu_tables = incomplete_eta_fu_tables,
#'                                     exemplar_lists = el,
#'                                     completed_fu_allocation_tables = fu_allocation_data,
#'                                     countries = "GHA")
#' # Show that the missing rows have been picked up from the exemplar country, ZAF.
#' completed %>%
#'   dplyr::filter(Country == "GHA", Machine == "Wood cookstoves")
assemble_eta_fu_tables <- function(incomplete_eta_fu_tables,
                                   exemplar_lists,
                                   completed_fu_allocation_tables,
                                   countries,
                                   years = NULL,
                                   which_quantity = c(IEATools::template_cols$eta_fu),
                                   country = IEATools::iea_cols$country,
                                   method = IEATools::iea_cols$method,
                                   energy_type = IEATools::iea_cols$energy_type,
                                   last_stage = IEATools::iea_cols$last_stage,
                                   unit = IEATools::iea_cols$unit,
                                   year = IEATools::iea_cols$year,
                                   e_dot = IEATools::iea_cols$e_dot,
                                   
                                   machine = IEATools::template_cols$machine,
                                   eu_product = IEATools::template_cols$eu_product,
                                   eta_fu = IEATools::template_cols$eta_fu,
                                   phi_u = IEATools::template_cols$phi_u,
                                   c_source = IEATools::template_cols$c_source,
                                   eta_fu_source = IEATools::template_cols$eta_fu_source,
                                   e_dot_machine = IEATools::template_cols$e_dot_machine,
                                   e_dot_machine_perc = IEATools::template_cols$e_dot_machine_perc,
                                   quantity = IEATools::template_cols$quantity,
                                   maximum_values = IEATools::template_cols$maximum_values,
                                   e_dot_perc = IEATools::template_cols$e_dot_perc,
                                   
                                   exemplars = PFUWorkflow::exemplar_names$exemplars,
                                   exemplar_tables = PFUWorkflow::exemplar_names$exemplar_tables,
                                   alloc_data = PFUWorkflow::exemplar_names$alloc_data,
                                   incomplete_eta_tables = PFUWorkflow::exemplar_names$incomplete_eta_table,
                                   complete_eta_tables = PFUWorkflow::exemplar_names$complete_eta_table,
                                   
                                   .values = IEATools::template_cols$.values) {
  
  which_quantity <- match.arg(which_quantity, several.ok = FALSE)
  
  # The FU allocation tables and the incomplete efficiency tables are easier to deal with when they are tidy.
  tidy_incomplete_eta_fu_tables <- IEATools::tidy_eta_fu_table(incomplete_eta_fu_tables,
                                                               year = year,
                                                               e_dot_machine = e_dot_machine,
                                                               e_dot_machine_perc = e_dot_machine_perc,
                                                               quantity = quantity,
                                                               maximum_values = maximum_values,
                                                               .values = .values)
  tidy_allocation_tables <- IEATools::tidy_fu_allocation_table(completed_fu_allocation_tables,
                                                               year = year,
                                                               e_dot = e_dot,
                                                               e_dot_perc = e_dot_perc,
                                                               quantity = quantity,
                                                               maximum_values = maximum_values,
                                                               .values = .values)
  if (!is.null(years)) {
    tidy_incomplete_eta_fu_tables <- tidy_incomplete_eta_fu_tables %>%
      dplyr::filter(.data[[year]] %in% years)
    tidy_allocation_tables <- tidy_allocation_tables %>%
      dplyr::filter(.data[[year]] %in% years)
  }
  
  completed_tables_by_year <- lapply(countries, FUN = function(coun) {
    coun_exemplar_strings <- exemplar_lists %>%
      dplyr::filter(.data[[country]] == coun)
    
    # For each combination of Country and Year (the rows of coun_exemplar_strings),
    # assemble a list of country allocation tables.
    coun_exemplar_strings_and_tables <- coun_exemplar_strings %>%
      dplyr::mutate(
        # Create a list column containing lists of exemplar tables
        # corresponding to the countries in the Exemplars column.
        "{exemplar_tables}" := Map(get_one_exemplar_table_list,
                                   # Need to wrap this in a list so the WHOLE table is sent via Map to get_one_exemplar_table_list
                                   tidy_incomplete_tables = list(tidy_incomplete_eta_fu_tables),
                                   exemplar_strings = .data[[exemplars]],
                                   yr = .data[[year]],
                                   country_colname = country,
                                   year_colname = year),
        # Add a column containing an FU allocation data frame for the country and year of each row
        "{alloc_data}" := Map(get_one_df_by_coun_and_yr,
                              .df = list(tidy_allocation_tables),
                              coun = .data[[country]],
                              yr = .data[[year]],
                              country_colname = country,
                              year_colname = year),
        # Add a column containing incomplete fu eta tables for each row (i.e., for each combination of country and year).
        "{incomplete_eta_tables}" := Map(get_one_df_by_coun_and_yr,
                                         .df = list(tidy_incomplete_eta_fu_tables),
                                         coun = .data[[country]],
                                         yr = .data[[year]],
                                         country_colname = country,
                                         year_colname = year),
        # Add a column containing completed fu efficiency tables for each row (i.e., for each combination of country and year).
        # Note that the data frames in this column contain the SOURCE of information for each efficiency
        "{complete_eta_tables}" := Map(IEATools::complete_eta_fu_table,
                                       eta_fu_table = .data[[incomplete_eta_tables]],
                                       exemplar_eta_fu_tables = .data[[exemplar_tables]],
                                       fu_allocation_table = .data[[alloc_data]],
                                       which_quantity = list(which_quantity),
                                       
                                       country = country,
                                       method = method,
                                       energy_type = energy_type,
                                       last_stage = last_stage,
                                       e_dot = e_dot,
                                       unit = unit,
                                       year = year,
                                       machine = machine,
                                       eu_product = eu_product,
                                       e_dot_perc = e_dot_perc,
                                       e_dot_machine = e_dot_machine,
                                       e_dot_machine_perc = e_dot_machine_perc,
                                       eta_fu = eta_fu,
                                       phi_u = phi_u,
                                       quantity = quantity,
                                       maximum_values = maximum_values,
                                       c_source = c_source,
                                       eta_fu_source = eta_fu_source,
                                       .values = .values)
      )
  }) %>%
    dplyr::bind_rows()
  
  # The only information we need to return is the completed efficiency tables.
  # Expand (un-nest) only the completed efficiency table column to give one data frame of all the FU efficiencies
  # for all years and all countries.
  completed_tables_by_year %>%
    dplyr::select(complete_eta_tables) %>%
    tidyr::unnest(cols = .data[[complete_eta_tables]])
}


#' Assemble completed phi (exergy-to-energy ratio) tables
#'
#' This function is used in the drake workflow to assemble completed phi (exergy-to-energy ratio) tables
#' given a set of phi tables read from machine data files and a phi constants table.
#' The algorithm gives priority in this order:
#' 1. phi values from the `incomplete_phi_u_table` argument
#' 2. phi values from climatic temperatures
#' 3. phi values from the `phi_constants_table` argument
#'
#' Note that the needed phi values are taken from `completed_efficiency_table`
#' (when not `NULL`).
#' If `completed_efficiency_table` is `NULL`,
#' the needed phi values are taken from `incomplete_phi_u_table`,
#' meaning that any empty (`NA`) phi values are obtained from climatic temperatures or `phi_constants_table`.
#'
#' @param incomplete_phi_u_table A data frame of phi values read from machine efficiency and phi data files.
#'                               This data frame can be "incomplete," i.e., it can be missing
#'                               phi values.
#'                               The phi values from `phi_constants_table` will be used instead.
#' @param phi_constants_table A data frame of constant phi values with reasonable default values for all energy products.
#' @param completed_efficiency_table A data frame containing completed efficiency tables.
#'                                   This data frame identifies all useful products
#'                                   for which we need phi values.
#'                                   Default is `NULL`, meaning that missing (`NA`) values in `incomplete_phi_u_table`
#'                                   should be completed.
#' @param countries A vector of countries for which completed phi tables are to be assembled.
#' @param years The years for which analysis is desired. Default is `NULL`, meaning analyze all years.
#' @param country,year,product See `IEATools::iea_cols`.
#' @param machine,quantity,phi_u,.values,eu_product,eta_fu_source See `IEATools::template_cols`.
#' @param phi_colname,phi_source_colname,is_useful See `IEATools::phi_constants_names`.
#' @param eta_fu_tables,phi_constants See `PFUWorkflow::phi_sources`.
#'
#' @return A data frame of phi values for every combination of country, year, machine, destination, etc.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(IEATools)
#' library(magrittr)
#' phi_constants_table <- IEATools::load_phi_constants_table()
#' # Load a phi_u_table.
#' phi_table <- IEATools::load_eta_fu_data() %>%
#'   # Convert to tidy format.
#'   dplyr::mutate(
#'     "{IEATools::template_cols$maximum_values}" := NULL,
#'     "{IEATools::iea_cols$unit}" := NULL
#'   ) %>%
#'   tidyr::pivot_longer(cols = IEATools::year_cols(.),
#'                       names_to = IEATools::iea_cols$year,
#'                       values_to = IEATools::template_cols$.values) %>%
#'   # Convert to a table of phi values only
#'   dplyr::filter(.data[[IEATools::template_cols$quantity]] == IEATools::template_cols$phi_u)
#' # Set a value to NA (Charcoal stoves, MTH.100.C, GHA, 1971) in the phi table.
#'   incomplete_phi_table <- phi_table %>%
#'     dplyr::mutate(
#'       "{IEATools::template_cols$.values}" := dplyr::case_when(
#'         .data[[IEATools::iea_cols$country]] == "GHA" &
#'         .data[[IEATools::iea_cols$year]] == 1971 &
#'         .data[[IEATools::template_cols$machine]] == "Charcoal stoves" ~ NA_real_,
#'         TRUE ~ .data[[IEATools::template_cols$.values]]
#'       )
#'     )
#' # Run through the assemble_phi_u_tables function
#' completed_phi_u_table <- assemble_phi_u_tables(incomplete_phi_table,
#'                                                phi_constants_table,
#'                                                countries = "GHA")
#' # Show that Charcoal stoves was filled
#' completed_phi_u_table %>%
#'   dplyr::filter(.data[[IEATools::template_cols$machine]] == "Charcoal stoves")
assemble_phi_u_tables <- function(incomplete_phi_u_table,
                                  phi_constants_table,
                                  completed_efficiency_table = NULL,
                                  countries,
                                  years = NULL,
                                  country = IEATools::iea_cols$country,
                                  year = IEATools::iea_cols$year,
                                  product = IEATools::iea_cols$product,
                                  machine = IEATools::template_cols$machine,
                                  quantity = IEATools::template_cols$quantity,
                                  phi_u = IEATools::template_cols$phi_u,
                                  .values = IEATools::template_cols$.values,
                                  eu_product = IEATools::template_cols$eu_product,
                                  eta_fu_source = IEATools::template_cols$eta_fu_source,
                                  phi_colname = IEATools::phi_constants_names$phi_colname,
                                  phi_source_colname = IEATools::phi_constants_names$phi_source_colname,
                                  is_useful = IEATools::phi_constants_names$is_useful_colname,
                                  eta_fu_tables = PFUWorkflow::phi_sources$eta_fu_tables,
                                  phi_constants = PFUWorkflow::phi_sources$phi_constants) {
  
  if (!is.null(years)) {
    incomplete_phi_u_table <- incomplete_phi_u_table %>%
      dplyr::filter(.data[[year]] %in% years)
  }
  
  completed_phi_tables_by_year <- lapply(countries, FUN = function(coun) {
    
    # Get a data frame of needed phi_u cases.
    # There are two potential sources of the needed phi_u cases.
    # First is a completed_efficiency_table,
    # a data frame which tells us
    # all combinations of country, year, machine, etc.,
    # that make useful energy.
    # Every useful energy carrier needs a phi value.
    # If the completed_efficiency_table is NULL,
    # the second source of information is the incomplete_phi_u_table,
    # which may contain missing (i.e. NA) values.
    if (is.null(completed_efficiency_table)) {
      needed_phi_u_cases <- incomplete_phi_u_table %>%
        dplyr::filter(.data[[country]] == coun,
                      .data[[quantity]] == phi_u) %>%
        dplyr::mutate(
          "{.values}" := NULL
        )
    } else {
      needed_phi_u_cases <- completed_efficiency_table %>%
        dplyr::filter(.data[[country]] == coun) %>%
        dplyr::mutate(
          # The completed_effiiency_table will have eta_fu for its quantity.
          # We want phi_u
          "{quantity}" := phi_u,
          # Eliminate the phi_u_source column
          "{phi_source_colname}" := NULL,
          # Eliminate the eta_fu_source column. We will add a phi_u_source column later
          "{eta_fu_source}" := NULL,
          # Eliminate the .values column. It contains eta_fu values.
          "{.values}" := NULL
        )
    }
    
    # Get a data frame of extant phi_u values
    # from the efficiency tables.
    # relevant to the particular analysis
    # for this country.
    # This data frame comes from the incomplete_phi_u_table.
    # Thus, any phi values coming from the efficiency tables or machine data tables
    # will have first priority
    phi_u_from_eta_fu_tables <- incomplete_phi_u_table %>%
      dplyr::filter(.data[[country]] == coun,
                    .data[[quantity]] == phi_u,
                    !is.na(.data[[.values]])) %>%
      dplyr::mutate(
        "{phi_source_colname}" := eta_fu_tables
      )
    if (!is.null(completed_efficiency_table)) {
      # incomplete_phi_u_table may have more rows than we need.
      # I.e., it may contain rows for years/countries that do not exist in
      # completed_efficiency_table.
      # Having the extra rows may cause problems later, so filter out the unneeded rows here.
      # But only do this step if completed_efficiency_table is present.
      phi_u_from_eta_fu_tables <- dplyr::semi_join(phi_u_from_eta_fu_tables,
                                                   completed_efficiency_table,
                                                   by = setdiff(names(incomplete_phi_u_table),
                                                                c(.values, quantity)))
    }
    
    phi_u_table <- phi_u_from_eta_fu_tables
    
    # Figure out missing phi_u cases by anti joining needed and present
    missing_phi_u_cases <- dplyr::anti_join(needed_phi_u_cases, phi_u_table, by = names(needed_phi_u_cases)) %>%
      # Strip off the .values column, if present,
      dplyr::mutate(
        "{.values}" := NULL
      )
    
    
    # Fill the missing values
    
    # Second priority will come from country-level temperature data.
    # This capability has not yet been coded here.
    
    # Steps are:
    #   (1) Figure out which phi values are available from temperature data
    #   (2) Join phi values to missing_phi_values data frame
    #   (3) anti_join to find remaining missing values
    
    # phi_u_from_temperature <- missing_phi_u_cases %>%
    #   dplyr::left_join(phi_u_temperature_table, by = )
    
    # phi_u_table <- dplyr::bind_rows(present_phi_u_table, phi_u_from_temperature)
    # # Calculate remaining missing values
    # missing_phi_u_cases <- dplyr::anti_join(needed_phi_u_values, phi_u_table, by = names(needed_phi_u_values)) %>%
    #   # Strip off the .values column, if present,
    #   dplyr::mutate(
    #     "{.values}" := NULL
    #   )
    
    
    # Third priority will come from the phi_constants data frame.
    phi_u_from_phi_constants <- missing_phi_u_cases %>%
      # left_join to pick up the values from phi_constants_table
      dplyr::left_join(phi_constants_table %>%
                         # Use only the useful data in phi_constants_table
                         dplyr::filter(.data[[is_useful]]) %>%
                         # Strip off the is.useful column, as it is no longer necessary.
                         dplyr::mutate("{is_useful}" := NULL) %>%
                         # Rename the Product column to Eu.product to match found_phi_values
                         dplyr::rename("{eu_product}" := .data[[product]]),
                       by = eu_product) %>%
      # At this point, we have the wrong column name.
      # Rename to match the expected column name.
      dplyr::rename("{.values}" := .data[[phi_colname]]) %>%
      # Add that these phi values came from the constants table
      dplyr::mutate(
        "{phi_source_colname}" := phi_constants
      ) %>%
      # Eliminate cases that are still missing.
      dplyr::filter(!is.na(.data[[.values]]))
    
    phi_u_table <- dplyr::bind_rows(phi_u_table, phi_u_from_phi_constants)
    
    
    # Calculate remaining missing values
    still_missing <- dplyr::anti_join(needed_phi_u_cases, phi_u_table, by = names(needed_phi_u_cases)) %>%
      # Strip off the .values column, if present.
      dplyr::mutate(
        "{.values}" := NULL
      )
    
    # Ensure that ALL rows have a value in .values
    if (nrow(still_missing) > 0) {
      err_msg <- paste("Not all useful energy carriers have been assigned phi values in assemble_phi_u_tables(). Missing combinations are:",
                       still_missing %>%
                         dplyr::select(.data[[country]], .data[[year]], .data[[machine]], .data[[eu_product]]) %>%
                         matsindf::df_to_msg())
      stop(err_msg)
    }
    
    # Now rbind everything together and return
    return(phi_u_table)
  }) %>%
    dplyr::bind_rows()
}
