% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{target_names}
\alias{target_names}
\title{Targets cache target names}
\format{
A string list with 48 entries.
\describe{
\item{countries}{The countries to be analyzed in this pipeline.}
\item{additional_exemplar_countries}{The additional exemplar countries from which allocations and efficiencies can be obtained.}
\item{alloc_and_eff_countries}{The allocation and efficiency countries target in the \code{targets} cache, giving the countries from which final-to-useful allocations and efficiencies can be drawn for final-to-useful analyses. This is a superset of countries to be analyzed.}
\item{years}{The years to be analyzed.}
\item{iea_data_path}{The path to the file containing IEA extended energy balance data.}
\item{country_concordance_path}{The path to the country concordance file.}
\item{phi_constants_path}{The path to exergy-to-energy ration (phi) constants.}
\item{ceda_data_folder}{The path to the folder containing CEDA data.}
\item{machine_data_path}{The path to the folder containing machine data excel files.}
\item{exemplar_table_path}{The path to the exemplar table.}
\item{fu_analysis_folder}{The path to a folder containing final-to-useful analyses for various countries.}
\item{report_source_folders}{The path to a folder containing reports to be run as the last step of the pipeline.}
\item{report_dest_folder}{The path to a folder containing reports from the pipeline.}
\item{pipeline_caches_folder}{The path to a folder containing pipeline caches for each release.}
\item{pipeline_releases_folder}{The path to a folder containing releases of targets.}
\item{release}{A boolean telling whether a release was requested.}
\item{iea_data}{The IEA data for \code{countries}.}
\item{country_concordance_table}{A data frame containing concordance information which maps full country names to custom 3 letter codes.}
\item{ceda_data}{The name of the data frame containing all CEDA temperature data read from \code{ceda_data_folder}.}
\item{all_machine_data}{A data frame containing eta_fu values.}
\item{machine_data}{A filtered version of \code{all_machine_data} containing information for only \code{alloc_and_eff_countries}.}
\item{socio_econ_data}{A data frame containing socioeconomic data, supplied by \code{get_L_K_GDP_data()}.}
\item{balanced_before}{A boolean indicating whether the \code{iea_data} are balanced before any further analysis. They usually are not, so this value is typically \code{FALSE}.}
\item{balanced_iea_data}{A balanced version of \code{iea_data}.}
\item{balanced_after}{Same as \code{balanced_before}, only for after balancing. This should always be be \code{TRUE}.}
\item{ok_to_proceed}{A boolean telling whether we can continue the pipeline. This target errors if \code{balanced_after} is not \code{TRUE}.}
\item{specified}{A data frame containing specified IEA data.}
\item{psut_final}{A data frame containing \code{specified} in a PSUT format.}
\item{exemplar_lists}{A data frame of lists of exemplar countries for each country in \code{countries}, and maybe more.}
\item{phi_constants}{A data frame of constant values for exergy-to-energy ratio (phi).}
\item{incomplete_allocation_tables}{A data frame of final-to-useful allocation tables, one for each country. These allocation tables may be incomplete.}
\item{completed_allocation_tables}{A data frame of completed final-to-useful allocation tables.}
\item{completed_efficiency_tables}{A data frame of completed final-to-useful efficiency tables.}
\item{completed_phi_u_tables}{A data frame of completed useful-stage exergy-to-energy ratios.}
\item{cmats}{A data frame containing \code{CompletedAllocationTables} in matrix form.}
\item{eta_fu_phi_u_vecs}{A data frame containing final-to-useful efficiency vectors and useful exergy-to-energy ratios.}
\item{eta_fu_vecs}{A data frame containing final-to-useful efficiency vectors.}
\item{phi_u_vecs}{A data frame containing useful exergy-to-energy ratio vectors.}
\item{phi_pf_vecs}{A data frame containing primary and final exergy-to-energy ratio vectors.}
\item{phi_vecs}{A data frame containing exergy-to-energy ratio vectors.}
\item{psut_useful}{A data frame containing PSUT matrices up to the useful stage.}
\item{psut}{A data frame containing PSUT matrices up to the useful stage and with exergy.}
\item{allocation_graphs}{A data frame containing final-to-useful allocation graphs.}
\item{non_stationary_allocation_graphs}{A data frame containing final-to-useful allocation graphs, for non-stationary data only.}
\item{efficiency_graphs}{A data frame containing final-to-useful efficiency graphs.}
\item{phi_graphs}{A data frame containing exergy-to-energy ratio (phi) graphs.}
\item{release_psut}{A target that does a release of the \code{psut} target. Contains the name of the target or a message saying that a release was not requested.}
\item{store_cache}{A target that stores the targets cache. Contains the name of the target or a message saying that a release was not requested.}
}
}
\usage{
target_names
}
\description{
A string list containing names of \code{targets} targets.
}
\examples{
target_names
}
\keyword{datasets}
