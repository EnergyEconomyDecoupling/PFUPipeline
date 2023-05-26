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
#' @param pin_name The name of the pin in the pinboard. `pin_name` is the key to retrieving `targ`.
#' @param type The type of the target, routed to `pins::pin_write()`. Default is "rds". "csv" in another option.
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
#' pinboard <- pins::board_folder("~/Dropbox/Fellowship 1960-2015 PFU database/OutputData/PipelineReleases/")
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
release_target <- function(pipeline_releases_folder, targ, pin_name, type = "rds", release = FALSE) {
  if (release) {
    # Establish the pinboard
    out <- pins::board_folder(pipeline_releases_folder, versioned = TRUE) %>%
      # Returns the fully-qualified name of the file written to the pinboard.
      pins::pin_write(targ, name = pin_name, type = type, versioned = TRUE)
  } else {
    out <- "Release not requested."
  }
  return(out)
}


#' Save the cache to a zip file, then to `pipeline_caches_folder`
#'
#' Saves a pipeline cache to a zip file into the `pipeline_caches_folder`.
#'
#' Note that the `dependency` argument is not used internally.
#' Rather, `dependency` exists to ensure that the pipeline
#' executes the right targets before saving the cache.
#'
#' @param pipeline_caches_folder The folder into which the pipeline cache will be saved as a .zip file.
#' @param cache_folder The cache folder that is to be zipped and saved.
#'                     This path is interpreted relative to the working directory.
#' @param file_prefix The prefix for the output file name.
#' @param dependency The last target that should be executed before saving the cache.
#'                   Not used internally.
#' @param release A boolean telling whether to do a release. The cache is stored only for releases.
#'                Default is `FALSE`.
#'
#' @return If the cache was saved, the file name is returned.
#'         If `release = FALSE`, the string "Release not requested." is returned.
#'
#' @export
stash_cache <- function(pipeline_caches_folder, cache_folder, file_prefix, dependency, release = FALSE) {
  if (!release) {
    return("Release not requested.")
  }
  # Zip the drake cache
  zipped_cache_filename <- paste0(file_prefix, parsedate::format_iso_8601(Sys.time()), ".zip") %>%
    # Change file name format to be equivalent to the pins file format.
    # Eliminate "-" characters
    gsub(pattern = "-", replacement = "") %>%
    # Eliminate ":" characters, because they cause problems on some OSes.
    gsub(pattern = ":", replacement = "") %>%
    # Change "+0000" to "Z", where "Z" means Zulu time (GMT offset of 00:00)
    gsub(pattern = "\\+0000", replacement = "Z")
  invisible(utils::zip(zipfile = zipped_cache_filename, files = cache_folder, extras = "-q"))
  # Calculate the folder structure for the output
  year <- lubridate::year(Sys.Date())
  month <- lubridate::month(Sys.Date())
  month <- sprintf("%02d", month)
  output_year_dir <- file.path(pipeline_caches_folder, year)
  dir.create(output_year_dir, showWarnings = FALSE)
  output_month_dir <- file.path(output_year_dir, month)
  dir.create(output_month_dir, showWarnings = FALSE)
  # Copy the file to the workflow output folder
  copy_successful <- file.copy(from = zipped_cache_filename,
                               to = output_month_dir)
  if (!copy_successful) {
    stop(paste("copying of pipeline cache unsuccessful in stach_cache():",
               zipped_cache_filename))
  }
  if (file.exists(zipped_cache_filename)) {
    # To keep things clean
    file.remove(zipped_cache_filename)
  }
  return(file_prefix)
}
