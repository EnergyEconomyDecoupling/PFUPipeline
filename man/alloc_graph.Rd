% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_functions.R
\name{alloc_graph}
\alias{alloc_graph}
\title{Generate an allocation graph}
\usage{
alloc_graph(
  .df,
  country,
  ef_product,
  destination,
  year = IEATools::iea_cols$year,
  .values = IEATools::template_cols$.values,
  machine = IEATools::template_cols$machine,
  eu_product = IEATools::template_cols$eu_product,
  machine_eu_product = paste0(machine, "_", eu_product)
)
}
\arguments{
\item{.df}{A data frame comprised of completed final energy allocations.}

\item{country}{The country for which this graph applies.}

\item{ef_product}{The final energy product for which this graph applies.}

\item{destination}{The destination sector for the final energy product.}

\item{year}{See \code{IEATools::iea_cols}.}

\item{.values, machine, eu_product}{See \code{IEATools::template_cols}.}

\item{machine_eu_product}{The name of a combined \code{machine} and \code{eu_product} column.}
}
\value{
A \code{ggplot2} graph object.
}
\description{
Creates an allocation graph from a completed allocation table,
mostly likely created by the \code{assemble_fu_allocation_tables()} function.
}
\details{
This function is called repeatedly from \code{alloc_plots_df()}.

\code{country}, \code{ef_product}, and \code{destination} form the title of the graph.
}
\examples{
library(ggplot2)
# Make a simple data frame with the expected structure.
tibble::tribble(~Year, ~.values, ~Machine, ~Eu.product,
                1967, 0.5, "Cars", "MD",
                1967, 0.5, "Industry static engines", "MD",
                2020, 0.8, "Cars", "MD",
                2020, 0.2, "Industry static engines", "MD") \%>\%
  alloc_graph(country = "Example", ef_product = "Petrol", destination = "Transport")
}
