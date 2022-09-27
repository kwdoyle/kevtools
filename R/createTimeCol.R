#' create uniformity across the datetime columns for all the redcap databases to load.
#' this function is meant to be used internally by loadRedcap
#'
#' @param x a list of dataframes to modify the datetime column for
#' @export


createTimeCol <- function(x) {
  if (any(names(x) == "eeg_date")) {
    # create new test_datetime
    rcfgtimes <- x$eeg_time
    rcfgtdatetimes <- paste(x$eeg_date, x$eeg_time, sep=" ")
    # just turn the NAs to 00:00s
    rcfgtdatetimes2 <- gsub("NA", "00:00", rcfgtdatetimes)
    # this is from turning the double blank "NA" strings pasted together that both were converted to 00:00s
    # rcfgdatetimes3 <- as.POSIXct(cleanDate(rcfgtdatetimes2, badstr="00:00 00:00"))
    # should be able to use the actual internals of the cleanDate function instead
    rcfgdatetimes3 <- as.POSIXct(ifelse(x == "00:00 00:00", NA, x))

    x$test_datetime <- rcfgdatetimes3
  } else if (any(names(x) == "test_datetime")) {
    x$test_datetime <- as.POSIXct(x$test_datetime)
  } else if (any(names(x) == "eeg_datetime")) {
    names(x)[which(names(x) == "eeg_datetime")] <- "test_datetime"
    x$test_datetime <- as.POSIXct(x$test_datetime)
  }
  return(x)
}
