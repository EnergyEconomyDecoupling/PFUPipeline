#' Generate an allocation graph
#'
#' Creates an allocation graph from a completed allocation table,
#' mostly likely created by the `assemble_fu_allocation_tables()` function.
#'
#' This function is called repeatedly from `alloc_plots_df()`.
#'
#' `country`, `ef_product`, and `destination` form the title of the graph.
#'
#' @param .df A data frame comprised of completed final energy allocations.
#' @param country The country for which this graph applies.
#' @param ef_product The final energy product for which this graph applies.
#' @param destination The destination sector for the final energy product.
#' @param year See `IEATools::iea_cols`.
#' @param .values,machine,eu_product See `IEATools::template_cols`.
#' @param machine_eu_product The name of a combined `machine` and `eu_product` column.
#'
#' @return A `ggplot2` graph object.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Make a simple data frame with the expected structure.
#' tibble::tribble(~Year, ~.values, ~Machine, ~Eu.product,
#'                 1967, 0.5, "Cars", "MD",
#'                 1967, 0.5, "Industry static engines", "MD",
#'                 2020, 0.8, "Cars", "MD",
#'                 2020, 0.2, "Industry static engines", "MD") %>%
#'   alloc_graph(country = "Example", ef_product = "Petrol", destination = "Transport")
alloc_graph <- function(.df,
                        country,
                        ef_product,
                        destination,
                        year = IEATools::iea_cols$year,
                        .values = IEATools::template_cols$.values,
                        machine = IEATools::template_cols$machine,
                        eu_product = IEATools::template_cols$eu_product,
                        machine_eu_product = paste0(machine, "_", eu_product)) {
  .df %>%
    dplyr::mutate(
      "{machine_eu_product}" := paste(.data[[machine]], "->", .data[[eu_product]])
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_area(mapping = ggplot2::aes(x = .data[[year]],
                                              y = .data[[.values]],
                                              group = .data[[machine_eu_product]],
                                              fill = .data[[machine_eu_product]]),
                       position = "fill") +
    ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::ylab("Allocation [-]") +
    ggplot2::ggtitle(paste0(c(country,
                              paste(ef_product, "->", destination)), collapse = "\n")) +
    MKHthemes::xy_theme() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   plot.title   = ggplot2::element_text(colour = "gray50", size = 10),
                   legend.text  = ggplot2::element_text(size = 10))
}


#' Create allocation graphs in a data frame
#'
#' This function adds a column of `ggplot2` graphs to a completed allocation tables data frame.
#' The graphs are stored in a list column named `plots`.
#'
#' The data frame is grouped by all variables needed to create the allocation graph, specifically
#' `country`, `ef_product`, and `destination` and nested prior to making the graphs, namely
#' `machine`, `eu_product`, `quantity`, `year`, `.values`, and `c_source`,
#' meaning that one allocation graph is constructed for each combination of those variables.
#'
#' @param .df The completed allocation tables data frame. Default is `drake::readd(completed_allocation_tables_target, path = cache_path, character_only = TRUE)`.
#' @param countries The countries for which allocation plots are to be created.
#' @param data_col The name of the output column containing nested data for allocation graphs. Default is "Data".
#' @param plots The name of the output column containing allocation graphs. Default is "Plots".
#' @param country See `IEATools::iea_cols`.
#' @param ef_product,destination,quantity,c_source See `IEATools::template_cols`.
#' @param year See `IEATools::iea_cols`. Passed to `alloc_graph()`.
#' @param .values,machine,eu_product See `IEATools::template_cols`. Passed to `alloc_graph()`.
#'
#' @return A data frame containing a list column of `ggplot2` allocation graphs.
#'
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#' # Make a simple data frame with the expected structure.
#' alloc_table <- tibble::tribble(~Country, ~Method, ~Energy.type, ~Year, ~Ef.product, ~Destination,
#'                                ~.values, ~Machine, ~Quantity, ~Eu.product, ~C.source,
#'                                "GHA", "PCM", "E", 1971, "Gasoline", "Transport",
#'                                0.5, "Cars", "C_1 [%]", "MD", "World",
#'                                "GHA", "PCM", "E", 1971, "Gasoline", "Transport",
#'                                0.5, "Trucks", "C_2 [%]", "MD", "World",
#'                                "GHA", "PCM", "E", 2020, "Gasoline", "Transport",
#'                                0.2, "Cars", "C_1 [%]", "MD", "World",
#'                                "GHA", "PCM", "E", 2020, "Gasoline", "Transport",
#'                                0.8, "Trucks", "C_2 [%]", "MD", "World",
#'                                "ZAF", "PCM", "E", 1971, "Gasoline", "Transport",
#'                                0.5, "Cars", "C_1 [%]", "MD", "World",
#'                                "ZAF", "PCM", "E", 1971, "Gasoline", "Transport",
#'                                0.5, "Trucks", "C_2 [%]", "MD", "World",
#'                                "ZAF", "PCM", "E", 2020, "Gasoline", "Transport",
#'                                0.3, "Cars", "C_1 [%]", "MD", "World",
#'                                "ZAF", "PCM", "E", 2020, "Gasoline", "Transport",
#'                                0.7, "Trucks", "C_2 [%]", "MD", "World")
#' alloc_plots_df(alloc_table, c("GHA", "ZAF"))
alloc_plots_df <- function(.df,
                           countries,
                           data_col = "Data",
                           plots = "Plots",
                           country = IEATools::iea_cols$country,
                           ef_product = IEATools::template_cols$ef_product,
                           destination = IEATools::template_cols$destination,
                           quantity = IEATools::template_cols$quantity,
                           c_source = IEATools::template_cols$c_source,
                           year = IEATools::iea_cols$year,
                           .values = IEATools::template_cols$.values,
                           machine = IEATools::template_cols$machine,
                           eu_product = IEATools::template_cols$eu_product) {
  
  .df %>%
    dplyr::filter(.data[[country]] %in% countries) %>%
    # Delete C.source column if it exists, because we don't need it for the graph,
    # and if it doesn't exist, we don't want to halt execution when an error is
    # thrown because we can't nest by a missing column.
    dplyr::mutate(
      "{c_source}" := NULL
    ) %>%
    matsindf::group_by_everything_except(machine, eu_product, quantity, year, .values) %>%
    tidyr::nest() %>%
    # The nest function creates a column called "data".
    # We want to rename it to the name specified by the caller.
    dplyr::rename(
      "{data_col}" := data
    ) %>%
    
    dplyr::mutate(
      "{plots}" := purrr::map(.x = .data[[data_col]], .f = alloc_graph,
                              country = .data[[country]], ef_product = .data[[ef_product]], destination = .data[[destination]],
                              year = year, .values = .values, machine = machine, eu_product = eu_product)
    )
}


#' Generate an allocation graph which contains non-stationary allocations data only
#'
#' Creates an allocation graph from a completed allocation table,
#' mostly likely created by the `assemble_fu_allocation_tables()` function.
#'
#' This function is called repeatedly from `nonstat_alloc_plots_df()`.
#'
#' `country`, `ef_product`, and `destination` form the title of the graph.
#'
#' @param .df A data frame comprised of completed final energy allocations.
#' @param country The country for which this graph applies.
#' @param ef_product The final energy product for which this graph applies.
#' @param destination The destination sector for the final energy product.
#' @param year See `IEATools::iea_cols`.
#' @param .values,machine,eu_product See `IEATools::template_cols`.
#' @param machine_eu_product The name of a combined `machine` and `eu_product` column.
#'
#' @return A `ggplot2` graph object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Make a simple data frame with the expected structure.
#' tibble::tribble(~Year, ~.values, ~Machine, ~Eu.product,
#'                 1967, 0.5, "Cars", "MD",
#'                 1967, 0.5, "Industry static engines", "MD",
#'                 2020, 0.8, "Cars", "MD",
#'                 2020, 0.2, "Industry static engines", "MD") %>%
#'   alloc_graph(country = "Example", ef_product = "Petrol", destination = "Transport")

nonstat_alloc_graph  <- function(.df,
                                 country,
                                 ef_product,
                                 destination,
                                 year = IEATools::iea_cols$year,
                                 .values = IEATools::template_cols$.values,
                                 machine = IEATools::template_cols$machine,
                                 eu_product = IEATools::template_cols$eu_product,
                                 machine_eu_product = paste0(machine, "_", eu_product)) {
  .df %>%
    dplyr::mutate(
      "{machine_eu_product}" := paste(.data[[machine]], "->", .data[[eu_product]])) %>%
    
    # dplyr::filter(Year < 2010) %>%
    
    ggplot2::ggplot() +
    ggplot2::geom_area(mapping = ggplot2::aes(x = .data[[year]],
                                              y = .data[[.values]],
                                              group = .data[[machine_eu_product]],
                                              fill = .data[[machine_eu_product]]),
                       position = "fill") +
    ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::ylab("Allocation [-]") +
    ggplot2::ggtitle(paste0(c(country,
                              paste(ef_product, "->", destination)),collapse = "\n")) +
    MKHthemes::xy_theme() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   plot.title   = ggplot2::element_text(colour = "gray50", size = 10),
                   legend.text  = ggplot2::element_text(size = 10))
}


#' Create non-stationary allocation graphs in a data frame
#'
#' This function adds a column of `ggplot2` graphs to a completed allocation tables data frame.
#' The graphs are stored in a list column named `plots`.
#'
#' The data frame is grouped by all variables needed to create the allocation graph, specifically
#' `country`, `ef_product`, and `destination` and nested prior to making the graphs, namely
#' `machine`, `eu_product`, `quantity`, `year`, `.values`, and `c_source`,
#' meaning that one allocation graph is constructed for each combination of those variables.
#'
#' @param .df The completed allocation tables data frame. Default is `drake::readd(completed_allocation_tables_target, path = cache_path, character_only = TRUE)`.
#' @param countries The countries for which allocation plots are to be created.
#' @param data_col The name of the output column containing nested data for allocation graphs. Default is "Data".
#' @param plots The name of the output column containing allocation graphs. Default is "Plots".
#' @param country See `IEATools::iea_cols`.
#' @param ef_product,destination,quantity,c_source See `IEATools::template_cols`.
#' @param year See `IEATools::iea_cols`. Passed to `alloc_graph()`.
#' @param .values,machine,eu_product See `IEATools::template_cols`. Passed to `alloc_graph()`.
#'
#' @return A data frame containing a list column of `ggplot2` non-stationary allocation graphs.
#'
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#' # Make a simple data frame with the expected structure.
#' alloc_table <- tibble::tribble(~Country, ~Method, ~Energy.type, ~Year, ~Ef.product, ~Destination,
#'                                ~.values, ~Machine, ~Quantity, ~Eu.product, ~C.source,
#'                                "GHA", "PCM", "E", 1971, "Gasoline", "Transport",
#'                                0.5, "Cars", "C_1 [%]", "MD", "World",
#'                                "GHA", "PCM", "E", 1971, "Gasoline", "Transport",
#'                                0.5, "Trucks", "C_2 [%]", "MD", "World",
#'                                "GHA", "PCM", "E", 2020, "Gasoline", "Transport",
#'                                0.2, "Cars", "C_1 [%]", "MD", "World",
#'                                "GHA", "PCM", "E", 2020, "Gasoline", "Transport",
#'                                0.8, "Trucks", "C_2 [%]", "MD", "World",
#'                                "ZAF", "PCM", "E", 1971, "Gasoline", "Transport",
#'                                0.5, "Cars", "C_1 [%]", "MD", "World",
#'                                "ZAF", "PCM", "E", 1971, "Gasoline", "Transport",
#'                                0.5, "Trucks", "C_2 [%]", "MD", "World",
#'                                "ZAF", "PCM", "E", 2020, "Gasoline", "Transport",
#'                                0.3, "Cars", "C_1 [%]", "MD", "World",
#'                                "ZAF", "PCM", "E", 2020, "Gasoline", "Transport",
#'                                0.7, "Trucks", "C_2 [%]", "MD", "World")
#' alloc_plots_df(alloc_table, c("GHA", "ZAF"))
nonstat_alloc_plots_df <- function(.df,
                                   countries,
                                   data_col = "Data",
                                   plots = "Plots",
                                   country = IEATools::iea_cols$country,
                                   ef_product = IEATools::template_cols$ef_product,
                                   destination = IEATools::template_cols$destination,
                                   quantity = IEATools::template_cols$quantity,
                                   c_source = IEATools::template_cols$c_source,
                                   year = IEATools::iea_cols$year,
                                   .values = IEATools::template_cols$.values,
                                   machine = IEATools::template_cols$machine,
                                   eu_product = IEATools::template_cols$eu_product) {
  
  .df %>%
    dplyr::filter(.data[[country]] %in% countries) %>%
    
    dplyr::group_by(.data[[country]],
                    .data[[ef_product]],
                    .data[[destination]],
                    .data[[machine]],
                    .data[[eu_product]],
                    .data[[quantity]],
                    .data[[c_source]]) %>%
    
    dplyr::filter(length(unique(.values)) > 1) %>%
    
    dplyr::ungroup() %>%
    
    # dplyr::group_by(.data[[country]],
    #                 .data[[ef_product]],
    #                 .data[[destination]]) %>%
    # dplyr::group_by(.data[[country]],
    #                 .data[["Method"]],
    #                 .data[["Energy.type"]],
    #                 # .data[["Last.stage"]],
    #                 # .data[["Ledger.side"]],
    #                 # .data[["Flow.aggregation.point"]],
    #                 # .data[["Unit"]],
  #                 .data[[ef_product]],
  #                 .data[[destination]]) %>%
  # Delete C.source column if it exists, because we don't need it for the graph,
  # and if it doesn't exist, we don't want to halt execution when an error is
  # thrown because we can't nest by a missing column.
  dplyr::mutate(
    "{c_source}" := NULL
  ) %>%
    matsindf::group_by_everything_except(machine, eu_product, quantity, year, .values) %>%
    tidyr::nest() %>%
    # The nest function creates a column called "data".
    # We want to rename it to the name specified by the caller.
    dplyr::rename(
      "{data_col}" := data
    ) %>%
    
    # Delete C.source column if it exists, because we don't need it for the graph,
    # and if it doesn't exist, we don't want to halt execution when an error is
    # thrown because we can't nest by a missing column.
    # dplyr::mutate(
    #   "{c_source}" := NULL
    # ) %>%
    # tidyr::nest("{data_col}" := c(machine, eu_product, quantity, year, .values)) %>%
    dplyr::mutate(
      # "{plots}" := purrr::map(.x = data, .f = alloc_graph,
      #                         country = .data[[country]], ef_product = .data[[ef_product]], destination = .data[[destination]],
      #                         year = year, .values = .values, machine = machine, eu_product = eu_product)
      "{plots}" := purrr::map(.x = .data[[data_col]], .f = nonstat_alloc_graph,
                              country = .data[[country]], ef_product = .data[[ef_product]], destination = .data[[destination]],
                              year = year, .values = .values, machine = machine, eu_product = eu_product)
      # "{plots}" := purrr::map(.x = .data[[data_col]], .f = alloc_graph,
      #                         country = .data[[country]], ef_product = ef_product, destination = destination,
      #                         year = year, .values = .values, machine = machine, eu_product = eu_product)
      
    )
}


####################################################################################################################################

#' Generate an eta_fu graph
#'
#' Creates an eta_fu graph from a completed eta_fu table,
#' created by the `assemble_eta_fu_tables()` function.
#'
#' This function is called repeatedly from `eta_fu_plots_df()`.
#'
#' `machine`, and `destination` form the title of the graph.
#'
#' @param .df A data frame comprised of completed final to useful efficiency values - eta.fu
#' @param countries The countries for which efficiency plots are to be created.
#' @param country,year See `IEATools::iea_cols`.
#' @param .values,machine,quantity,eu_product See `IEATools::template_cols`.
#' @param machine_eu_product The name of a combined `machine` and `eu_product` column.
#'
#' @return A `ggplot2` graph object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Make a simple data frame with the expected structure.
#' tibble::tribble(~Year, ~.values, ~Machine, ~Eu.product,
#'                 1967, 0.5, "Cars", "MD",
#'                 1967, 0.5, "Industry static engines", "MD",
#'                 2020, 0.8, "Cars", "MD",
#'                 2020, 0.2, "Industry static engines", "MD") %>%
#'   alloc_graph(country = "Example", ef_product = "Petrol", destination = "Transport")
eta_fu_graph <- function(.df,
                         countries,
                         country = IEATools::iea_cols$country,
                         quantity = IEATools::template_cols$quantity,
                         year = IEATools::iea_cols$year,
                         .values = IEATools::template_cols$.values,
                         machine = IEATools::template_cols$machine,
                         eu_product = IEATools::template_cols$eu_product,
                         machine_eu_product = paste0(machine, "_", eu_product)) {
  
  
  the_machine <- .df[[machine]] %>%
    unique()
  assertthat::assert_that(length(the_machine) == 1,
                          msg = "Found more than 1 machine in eta_fu_graph().")
  
  the_eu_product <- .df[[eu_product]] %>%
    unique()
  assertthat::assert_that(length(the_eu_product) == 1,
                          msg = "Found more than 1 eu_product in eta_fu_graph().")
  
  .df %>%
    dplyr::mutate(
      "{machine_eu_product}" := paste(.data[[machine]], "->", .data[[eu_product]])
    ) %>%
    dplyr::filter(.data[[quantity]] == "eta.fu") %>%
    dplyr::filter(.data[[year]] < 2010) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[year]],
                                              y = .data[[.values]],
                                              color = .data[[country]])) +
    ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
    ggplot2::ylim(0, NA) +
    #ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::ylab("eta.fu [%]") +
    ggplot2::ggtitle(paste0(c(paste(the_machine, "->", the_eu_product), collapse = "\n"))) +
    MKHthemes::xy_theme() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   plot.title   = ggplot2::element_text(colour = "gray50", size = 10),
                   legend.text  = ggplot2::element_text(size = 10))
  
}


#' Create eta_fu graphs in a data frame
#'
#' This function adds a column of `ggplot2` graphs to a completed eta_fu tables data frame.
#' The graphs are stored in a list column named `plots`.
#'
#' By default, the completed eta_fu tables data frame is read from a `drake` cache.
#'
#' The data frame is grouped by `machine`, and `eu_product` and nested prior to making the graphs,
#' meaning that one final-to useful efficiency graph is constructed for each combination of machine, and eu_product.
#'
#' @param .df The completed final-to useful efficiency tables data frame, 
#'            which contains both eta.fu and phi.u values.
#' @param countries The countries for which final-to useful efficiency plots are to be created.
#' @param plots The name of the output column containing final-to useful efficiency graphs. 
#'              Default is "plots".
#' @param country See `IEATools::iea_cols`.
#' @param year See `IEATools::iea_cols`. Passed to `alloc_graph()`.
#' @param .values,machine,eu_product See `IEATools::template_cols`. Passed to `eta_fu_graph()`.
#' @param machine_eu_product The name of a combined `machine` and `eu_product` column.
#'
#' @return A data frame containing a list column of `ggplot2` final-to useful efficiency graphs.
#'
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#' # Make a simple data frame with the expected structure.
#' alloc_table <- tibble::tribble(~Country, ~Year, ~Ef.product, ~Destination,
#'                                ~.values, ~Machine, ~Eu.product,
#'                                "GHA", 1971, "Gasoline", "Transport",
#'                                0.5, "Cars", "MD",
#'                                "GHA", 1971, "Gasoline", "Transport",
#'                                0.5, "Trucks", "MD",
#'                                "GHA", 2020, "Gasoline", "Transport",
#'                                0.2, "Cars", "MD",
#'                                "GHA", 2020, "Gasoline", "Transport",
#'                                0.8, "Trucks", "MD",
#'                                "ZAF", 1971, "Gasoline", "Transport",
#'                                0.5, "Cars", "MD",
#'                                "ZAF", 1971, "Gasoline", "Transport",
#'                                0.5, "Trucks", "MD",
#'                                "ZAF", 2020, "Gasoline", "Transport",
#'                                0.3, "Cars", "MD",
#'                                "ZAF", 2020, "Gasoline", "Transport",
#'                                0.7, "Trucks", "MD")
#' alloc_plots_df(alloc_table, c("GHA", "ZAF"))
eta_fu_plots_df <- function(.df,
                            countries,
                            plots = "Plots", # CHANGED TO CAP
                            country = IEATools::iea_cols$country,
                            year = IEATools::iea_cols$year,
                            .values = IEATools::template_cols$.values,
                            machine = IEATools::template_cols$machine,
                            eu_product = IEATools::template_cols$eu_product,
                            machine_eu_product = paste0(machine, "_", eu_product)) {
  
  .df %>%
    dplyr::filter(.data[[country]] %in% countries) %>%
    dplyr::filter(.data[[machine]] != "Non-energy use") %>%
    dplyr::mutate(
      "{machine_eu_product}" := paste(.data[[machine]], "->", .data[[eu_product]])
    ) %>%
    dplyr::group_by(.data[[machine_eu_product]]) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      "{plots}" := purrr::map(data, eta_fu_graph)
    )
}

####################################################################################################################################

#' Generate an phi_u graph
#'
#' Creates an phi_u graph from a completed eta_fu table,
#' created by the `assemble_eta_fu_tables()` function.
#'
#' This function is called repeatedly from `phi_u_plots_df()`.
#'
#' `machine`, and `destination` form the title of the graph.
#'
#' @param .df A data frame comprised of completed exergy-to-energy ratio values
#' @param countries The countries for which efficiency plots are to be created.
#' @param country,year See `IEATools::iea_cols`.
#' @param .values,machine,quantity,eu_product See `IEATools::template_cols`.
#' @param machine_eu_product The name of a combined `machine` and `eu_product` column.
#'
#' @return A `ggplot2` graph object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Make a simple data frame with the expected structure.
#' tibble::tribble(~Year, ~.values, ~Machine, ~Eu.product,
#'                 1967, 0.5, "Cars", "MD",
#'                 1967, 0.5, "Industry static engines", "MD",
#'                 2020, 0.8, "Cars", "MD",
#'                 2020, 0.2, "Industry static engines", "MD") %>%
#'   alloc_graph(country = "Example", ef_product = "Petrol", destination = "Transport")
phi_u_graph <- function(.df,
                        countries,
                        country = IEATools::iea_cols$country,
                        quantity = IEATools::template_cols$quantity,
                        year = IEATools::iea_cols$year,
                        .values = IEATools::template_cols$.values,
                        machine = IEATools::template_cols$machine,
                        eu_product = IEATools::template_cols$eu_product,
                        machine_eu_product = paste0(machine, "_", eu_product)) {
  
  
  the_machine <- .df[[machine]] %>%
    unique()
  assertthat::assert_that(length(the_machine) == 1,
                          msg = "Found more than 1 machine in phi_u_graph().")
  
  the_eu_product <- .df[[eu_product]] %>%
    unique()
  assertthat::assert_that(length(the_eu_product) == 1,
                          msg = "Found more than 1 eu_product in phi_u_graph().")
  
  .df %>%
    dplyr::mutate(
      "{machine_eu_product}" := paste(.data[[machine]], "->", .data[[eu_product]])
    ) %>%
    dplyr::filter(.data[[quantity]] == "phi.u") %>%
    dplyr::filter(.data[[year]] < 2010) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[year]],
                                              y = .data[[.values]],
                                              color = .data[[country]])) +
    ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
    ggplot2::ylim(0, NA) +
    #ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::ylab("phi.u [%]") +
    ggplot2::ggtitle(paste0(c(paste(the_machine, "->", the_eu_product), collapse = "\n"))) +
    MKHthemes::xy_theme() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   plot.title   = ggplot2::element_text(colour = "gray50", size = 10),
                   legend.text  = ggplot2::element_text(size = 10))
  
}


#' Create phi_u graphs in a data frame
#'
#' This function adds a column of `ggplot2` graphs to a completed phi_u tables data frame.
#' The graphs are stored in a list column named `plots`.
#'
#' By default, the completed phi_u tables data frame is read from a `drake` cache.
#'
#' The data frame is grouped by `machine`, and `eu_product` and nested prior to making the graphs,
#' meaning that one exergy-to-energy ratio graph is constructed for each combination of machine, and eu_product.
#'
#' @param .df The completed final-to useful efficiency tables data frame, which contains both eta.fu and phi.u values.
#' @param countries The countries for which exergy-to-energy ratio plots are to be created.
#' @param plots The name of the output column containing exergy-to-energy ratio graph. Default is "plots".
#' @param country See `IEATools::iea_cols`.
#' @param year See `IEATools::iea_cols`. Passed to `alloc_graph()`.
#' @param .values,machine,eu_product See `IEATools::template_cols`. Passed to `eta_fu_graph()`.
#' @param machine_eu_product The name of a combined `machine` and `eu_product` column.
#'
#' @return A data frame containing a list column of `ggplot2` exergy-to-energy ratio graphs.
#'
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#' # Make a simple data frame with the expected structure.
#' alloc_table <- tibble::tribble(~Country, ~Year, ~Ef.product, ~Destination,
#'                                ~.values, ~Machine, ~Eu.product,
#'                                "GHA", 1971, "Gasoline", "Transport",
#'                                0.5, "Cars", "MD",
#'                                "GHA", 1971, "Gasoline", "Transport",
#'                                0.5, "Trucks", "MD",
#'                                "GHA", 2020, "Gasoline", "Transport",
#'                                0.2, "Cars", "MD",
#'                                "GHA", 2020, "Gasoline", "Transport",
#'                                0.8, "Trucks", "MD",
#'                                "ZAF", 1971, "Gasoline", "Transport",
#'                                0.5, "Cars", "MD",
#'                                "ZAF", 1971, "Gasoline", "Transport",
#'                                0.5, "Trucks", "MD",
#'                                "ZAF", 2020, "Gasoline", "Transport",
#'                                0.3, "Cars", "MD",
#'                                "ZAF", 2020, "Gasoline", "Transport",
#'                                0.7, "Trucks", "MD")
#' alloc_plots_df(alloc_table, c("GHA", "ZAF"))
phi_u_plots_df <- function(.df,
                           countries,
                           plots = "Plots", # CHANGED TO CAP
                           country = IEATools::iea_cols$country,
                           year = IEATools::iea_cols$year,
                           .values = IEATools::template_cols$.values,
                           machine = IEATools::template_cols$machine,
                           eu_product = IEATools::template_cols$eu_product,
                           machine_eu_product = paste0(machine, "_", eu_product)) {
  
  .df %>%
    dplyr::filter(.data[[country]] %in% countries) %>%
    dplyr::filter(.data[[machine]] != "Non-energy use") %>%
    dplyr::mutate(
      "{machine_eu_product}" := paste(.data[[machine]], "->", .data[[eu_product]])
    ) %>%
    dplyr::group_by(.data[[machine_eu_product]]) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      "{plots}" := purrr::map(data, phi_u_graph)
    )
}
