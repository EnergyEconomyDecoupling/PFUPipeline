---
title: "Release notes for `PFUDatabase`"
output: html_document
---


# PFUDatabase 0.1.0 (2022-04-14) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6462165.svg)](https://doi.org/10.5281/zenodo.6462165)

* Beginning to develop as a package and a `targets` workflow.
* Several pieces of the targets pipeline are now in place.


# PFUDatabase 0.0.9 (2022-04-03) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6409576.svg)](https://doi.org/10.5281/zenodo.6409576)

* Update names of constants in debugging file.


# PFUDatabase 0.0.8 (2022-02-24) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6264718.svg)](https://doi.org/10.5281/zenodo.6264718)

* Name change to `PFUDatabase`.
* Now using `pins` package for storing the `PSUT` target.


# PFU-Database 0.0.7 (2021-10-15) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5572434.svg)](https://doi.org/10.5281/zenodo.5572434)

* Added `phi_constants_path` to the _drake.R file.
  This enables use of many constant values for phi (the energy-to-exergy ratio), 
  taking advantage of new features in the PFUWorkflow package.
* Updated `debugging.R` for new targets.


# PFU-Database 0.0.6 (2021-08-20) [![DOI](https://zenodo.org/badge/239981862.svg)](https://zenodo.org/badge/latestdoi/239981862)

* Updated `debugging.R` for new `CountryConcordanceTable` target. 


# PFU-Database 0.0.5 (2021-01-24)

* Updated PFUWorkflow::get_plan() arguments to add a target for Machine data
* Updated PFUWorkflow::get_plan() arguments to add a target for CEDA temperature data
* Added working non-stationary final-to-useful allocations report.
* Added working exergy-to-energy ratio report.


# PFU-Database 0.0.4 (2020-09-28)

* Hotfix: Removed missed merge conflicts from 0.0.3


# PFU-Database 0.0.3 (2020-09-28)

* Added working final-useful efficiency report. Updated allocation report to include timestamp in file name.
* Added play button to Sankey diagram which runs through all years in an animation.
* Added tables of the unique machines, useful work products, and machine-useful work product combinations 
  to the "Framework" tab of the App.
* Added the ability to plot final-to-useful efficiencies vs GDP per Capita PPP


# PFU-Database 0.0.2 (2020-09-18)

* First version of PFU-Database which contains a working version of the Shiny App.


# PFU-Database 0.0.1 (2020-09-17)

* First version of PFU-Database that contains develop branch for all work in this repository.

