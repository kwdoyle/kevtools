#' Drop rows where every column specified is NA
#'
#' pretty self-explanatory
#' @param dat the input data frame
#' @param checkCols columns to check where data is missing
#' @export
#' @examples
#' newdat <- BetterDropNA(dat, checkCols=c("column1", "column3"))

BetterDropNA <- function(dat, checkCols) {
  out <- dat[rowSums(is.na(dat[,checkCols])) != ncol(dat[,checkCols]), ]
  return(out)
}
