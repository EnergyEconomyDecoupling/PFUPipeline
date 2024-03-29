% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efficiencies.R
\name{calc_fu_Y_EIOU_efficiencies}
\alias{calc_fu_Y_EIOU_efficiencies}
\title{Calculate final-to-useful efficiencies}
\usage{
calc_fu_Y_EIOU_efficiencies(
  C_mats,
  eta_fu_vecs,
  phi_vecs,
  countries,
  country = IEATools::iea_cols$country,
  last_stage = IEATools::iea_cols$last_stage,
  energy_type = IEATools::iea_cols$energy_type,
  method = IEATools::iea_cols$method,
  year = IEATools::iea_cols$year
)
}
\arguments{
\item{C_mats}{A data frame containing allocation matrices.}

\item{eta_fu_vecs}{A data frame containing vectors of machine efficiencies, probably the Etafuvecs target.}

\item{phi_vecs}{A data frame containing vectors of exergy-to-energy ratios, probably the Phivecs target.}

\item{countries}{The countries for which this analysis should be performed.}

\item{country, last_stage, energy_type, method, year}{See \code{IEATools::iea_cols}.}
}
\value{
A data frame of final-to-useful efficiencies by energy sector and energy carrier.
}
\description{
Knowing allocations (\code{C_mats}), machine efficiencies (\code{eta_m_vecs}), and
exergy-to-energy ratios (\code{phi_vecs}), it is possible to
calculate the final-to-useful efficiencies for all
final demand and energy industry own use
in an energy conversion chain.
This function performs those calculations using
\code{Recca::calc_eta_fu_Y_eiou()}.
}
