#' Prepare phi vectors for writing as a data frame
#' 
#' The Phivecs target is a `matsindf` data frame 
#' that contains a column of phi vectors.
#' But another common data format is a simple data frame.
#' This function converts the Phivecs matsindf data frame to 
#' a data table.
#'
#' @param phi_vecs The incoming Phivecs target.
#' @param machine,phi,matnames,matvals,rownames,colnames,rowtypes,coltypes Columns names in `phi_vecs`.
#'
#' @return A data frame with columns Country, Year, Machine, and phi.
#' 
#' @export
expand_phi_vecs <- function(phi_vecs, 
                            machine = "Machine", 
                            phi = "phi",
                            matnames = "matnames",
                            matvals = "matvals",
                            rownames = "rownames", 
                            colnames = "colnames", 
                            rowtypes = "rowtypes", 
                            coltypes = "coltypes") {
  phi_vecs |> 
    tidyr::pivot_longer(cols = "phi", names_to = "matnames", values_to = "matvals") |> 
    matsindf::expand_to_tidy() |> 
    dplyr::mutate(
      # Delete some columns we don't need.
      "{matnames}" := NULL, 
      "{colnames}" := NULL, 
      "{rowtypes}" := NULL, 
      "{coltypes}" := NULL
    ) |> 
    dplyr::rename(
      "{machine}" := dplyr::all_of(rownames), 
      "{phi}" := dplyr::all_of(matvals)
    )
}