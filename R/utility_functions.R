#' Retrieve a list of final demand sectors
#'
#' Retrieve a list of final demand sectors for calculation of the total final consumption
#' of final, useful, or services energy in gross or net terms.
#'
#' @return A list of final demand sectors from `IEATools::fd_sectors`.
#' 
#' @export
#'
#' @examples
#' fd_sectors <- get_fd_sectors()
get_fd_sectors <- function(){
  IEATools::fd_sectors
}


#' Retrieve primary industry prefixes
#'
#' Retrieve primary industry prefixes for use by `Recca::find_p_industry_names`.
#' Contains "Resources", "Imports", and "Stock changes".
#'
#' @return A list of primary industry prefixes from `IEATools::prim_agg_flows`.
#' 
#' @export
#'
#' @examples
#' p_industry_prefixes <- get_p_industry_prefixes()
get_p_industry_prefixes <- function() {
  IEATools::prim_agg_flows %>% unname() %>% unlist() %>% list()
}
