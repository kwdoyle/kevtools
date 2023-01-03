#' source in the latest version of data from a redcap database
#'
#' @param path a given path, or vector of paths, conatining a specific file or files to source
#' @param rcids a given path, or vector of paths, to the files containing the redcap ID to mrn link
#' @export


loadRedcap <- function(path, rcids) {
  library(stringr)
  library(readxl)
  library(gtools)
  library(sjlabelled)

  if (class(path) != "character" & class(rcids) != "list") {
    stop("Must provide vector of paths to redcap data folders and a list of their recdap ID files in order")
  }

  if (length(path) != length(rcids)) {
    stop("'path' and 'rcids' must be same length")
  }

  rcid_lst <- list()
  for (i in 1:length(rcids)) {
    rcidtmp <- read_xlsx(rcids[i])
    # the mrn column can have spaces before or after the mrn itself.
    # converting straight to numeric will generate NAs.
    # need to remove the whitespace, and only this function seems to work for some reason.
    rcidtmp$mrn <- str_trim(rcidtmp$mrn, side="both")
    rcidtmp$mrn <- as.character(rcidtmp$mrn)
    rcid_lst[[i]] <- rcidtmp
  }

  out <- list()
  for (i in 1:length(path)) {
    env <- new.env()
    kevtools::sourceEnv(path=path[i], env=env)
    data <- env$data
    data2 <- remove_all_labels(kevtools::processREDCapData(data))
    # remove specific bad characters from the ids
    if (any(grepl("A-", rcid_lst[[i]]$record_id))) {
      rcid_lst[[i]]$record_id <- gsub("A-", "", rcid_lst[[i]]$record_id)
    }
    # also this
    if (class(rcid_lst[[i]]$record_id) == "character") {
      data2$record_id <- as.character(data2$record_id)
    }
    # add mrns first here
    data2 <- data2 %>%
      left_join(select(rcid_lst[[i]], record_id, mrn), by = 'record_id')
    out[[i]] <- data2
  }

  # if any of the tables have this column, then perform this to fix and convert to 'test_datetime'
  chk1 <- sapply(out, function(x) any(names(x) == "test_datetime"))
  chk2 <- sapply(out, function(x) any(names(x) == "eeg_date"))
  # also need to check for THIS
  chk3 <- sapply(out, function(x) any(names(x) == "eeg_datetime"))

  if (any(chk1) | any(chk2) | any(chk3)) {
    out <- lapply(out, kevtools::createTimeCol)
  }

  # this just converts the test date back to character.
  data3 <- smartbind(list=out)
  # final clean of mrns/IDs
  data3$mrn <- ifelse(is.na(data3$mrn), data3$record_id, data3$mrn)

  return(data3)
}
