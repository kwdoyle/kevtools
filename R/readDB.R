#' Read a multi-sheet excel table into a list
#'
#' Takes what is essentially an excel database and reads all of the tables (i.e., sheets)
#' into a list.
#' @param filepath the filepath of the excel file
#' @export
#' @examples
#' path <- "path/to/file.xlsx"
#' out <- readDB(filepath=path)

readDB <- function(filepath) {
  library(openxlsx)

  out <- list()
  wb <- loadWorkbook(filepath)
  nms <- names(wb)
  for (i in 1:length(nms)) {
    out[[i]] <- readWorkbook(wb, sheet=nms[i])
  }
  names(out) <- nms

  return(out)
}
