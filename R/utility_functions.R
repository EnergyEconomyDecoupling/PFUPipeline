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
  
  fd_sectors <- IEATools::fd_sectors
  
  return(fd_sectors)
}
