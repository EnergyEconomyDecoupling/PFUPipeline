#' Save a target to a pinboard.
#'
#' Releases (`release = TRUE`)
#' or not (`release = FALSE`)
#' a new version of the target target
#' using the `pins` package.
#'
#' Released versions of the target can be obtained
#' as shown in examples.
#'
#' @param pipeline_releases_folder The folder that contains the pinboard for releases from the pipeline.
#' @param targ The target R object to be saved to the pinboard.
#' @param targ_name The name of the target object. `targ_name` is the key to retrieving `targ`.
#' @param type The type of the target, routed to `pins::pin_write()`. Default is "rds".
#' @param release A boolean telling whether to do a release.
#'                Default is `FALSE`.
#'
#' @return If `release` is `TRUE`,
#'         the fully-qualified path name of the `targ` file in the pinboard.
#'         If `release` is `FALSE`, the string "Release not requested."
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Establish the pinboard
#' pinboard <- pins::board_folder("~/Dropbox/Fellowship 1960-2015 PFU database/PipelineReleases/")
#' # Get information about the `PSUT` target in the pinboard
#' pinboard %>%
#'   pins::pin_meta(name = "psut")
#' # Find versions of the `PSUT` target
#' pinboard %>%
#'   pins::pin_versions(name = "psut")
#' # Get the latest copy of the `PSUT` target.
#' my_psut <- pinboard %>%
#'   pins::pin_read(name = "psut")
#' # Retrieve a previous version of the `PSUT` target.
#' my_old_psut <- pinboard %>%
#'   pins::pin_read(name = "psut", version = "20220218T023112Z-1d9e1")}
release_target <- function(pipeline_releases_folder, targ, targ_name, type = "rds", release = FALSE) {
  if (release) {
    # Establish the pinboard
    out <- pins::board_folder(pipeline_releases_folder, versioned = TRUE) %>%
      # Returns the fully-qualified name of the file written to the pinboard.
      pins::pin_write(targ, name = targ_name, type = type, versioned = TRUE)
  } else {
    out <- "Release not requested."
  }
  return(out)
}
