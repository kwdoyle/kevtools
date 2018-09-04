#' Read an "extended" excel file
#'
#' This will read in an excel file that has its data extend onto multiple sheets.
#' i.e., it's just one huge table that happens to be split up onto multiple sheets.
#' ...This hasn't ever been tested as a function before.
#' @param datfiles a vector of all of the filenames to be read
#' @export

readExtendedExcel <- function(datfiles) {
  library(openxlsx)

  alldat <- data.frame()

  for (i in 1:length(datfiles)) {
    print(paste("loading file", i))
    wb <- loadWorkbook(datfiles[i])
    nms <- names(wb)

    for (j in 1:length(nms)) {
      print(paste("loading sheet", j))
      if (j == 1) {
        sht <- readWorkbook(wb, sheet=nms[j], colNames = TRUE)
      } else {
        sht <- readWorkbook(wb, sheet=nms[j], colNames = FALSE)
        names(sht) <- names(alldat)
      }

      alldat <- rbind(alldat, sht)

    }
  }
}
