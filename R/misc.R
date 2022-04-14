#' Combine countries and additional exemplars
#'
#' @param couns Countries of interest
#' @param exempls Additional exemplars
#'
#' @return A unique combination of countries and additional exemplars
#' 
#' @export
combine_countries_exemplars <- function(couns, exempls) {
  c(couns, exempls) %>% 
    unique()
}