#' source the most recent file located in a given directory path.
#' this function is meant to be used internally within the loadRedcap function.
#'
#' @param path a given path, or vector of paths, conatining a specific file or files to source
#' @param env a new environment to source the file into. this is internally created in loadRedcap with new.env()
#' @export

sourceEnv <- function(path, env) {
  # This will find the most recent file in a directory and source it into its own environment to be accessed
  FILES <- list.files(path, pattern='*.R', full.names=T)
  details <-  file.info(FILES)
  details <- details[order(details$mtime, decreasing=T), ]
  fl <- rownames(details)[1]

  print(paste("sourcing file", fl))
  sys.source(fl, chdir=T, envir=env)
}
