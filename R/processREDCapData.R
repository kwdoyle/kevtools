#' Convert REDCap structured data into something actually usable
#'
#' @param data the raw data from redcap
#' @param redcap_blank the value for any missing data in redcap. defaults to ''
#' @export
#' @examples
#'
#' newdat <- processREDcapData(data=rawredcap, redcap_blank='')

processREDCapData <- function(data, redcap_blank='') {
  library(dplyr)
  # convert redcap blanks to nas
  data <- dplyr::mutate_all(data, list(~dplyr::na_if(., redcap_blank)))
  # get names of all repeated instruments, separate them all from the non-repeated instruments,
  # then join them. this way the data should be in the repeated format and any NAs per row should be gone
  rep_insts <- unique(data$redcap_repeat_instrument)
  rep_insts <- rep_insts[which(!is.na(rep_insts))]   # != redcap_blank)]

  # get non-repeat instance data
  # maindat <- data[which(data$redcap_repeat_instrument == redcap_blank), ]
  maindat <- data[which(is.na(data$redcap_repeat_instrument)), ]
  # remove repeat inst columns from maindat
  maindat <- Filter(function(x) !all(is.na(x)), maindat)
  # then loop over the repeat instances and join them to maindat
  for (inst in rep_insts) {
    # also remove the 'redcap repeated instument, instance" columns
    dumbcols <- which(names(data) %in% c("redcap_repeat_instrument", "redcap_repeat_instance",
                                         "redcap_repeat_instrument.factor", "redcap_repeat_instance.factor"))
    inst_dat_raw <- data[which(data$redcap_repeat_instrument == inst), -dumbcols]
    # wait I'm gonna have a ton of blank rows for the columns that don't appear in these repeated instances
    # ....are these columns completely blank anyway in the repeated instances?
    # looks like it. test these ways of filtering out all na columns
    inst_dat <- Filter(function(x) !all(is.na(x)), inst_dat_raw)
    # after I merge for the next rep inst, there should be 4 rows for record id 1
    # and there are. nice.
    maindat <- merge(maindat, inst_dat, by = 'record_id', all.x = TRUE)
  }

  return(maindat)

}
