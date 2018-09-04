#' Create a Microsoft SQL Server temporary table
#'
#' Copy an R data frame into Microsoft SQL Server as a temporary table which can then be used to query other SQL tables
#' @param chan the ODBC channel
#' @param data the data frame to copy to SQL
#' @param tbl_name the desired temporary table name, without the pound sign
#' @param columns the columns from data wished to be copied
#' @param dtypes the data types of those columns
#' @export
#' @examples
#' library(RODBC)
#'
#' chan <- odbcConnect("AthensSql")
#' # if pulling data from Bedmaster, all dates should be unix epochs, which is just an integer.
#' MakeSQLTempTable(chan, data, tbl_name="patients", columns=c("MRN", "Test_Date"), dtypes=c("integer", "integer"))

MakeSQLTempTable <- function(chan, data, tbl_name, columns, dtypes) {
  # make sure data is a normal data frame first
  data <- as.data.frame(data)
  ## set up the table
  lines <- paste(columns, dtypes, collapse=",")
  #cols <- paste(shQuote(columns, type="csh"), collapse=",")
  cols <- paste(columns, collapse=",")
  qry <- paste0(
    "CREATE TABLE #", tbl_name, " (",
    lines, ")"
  )

  sqlQuery(chan, qry)


  # round any numeric columns to avoid mysterious decimals being placed after using shQuote
  for (col in columns) {
    if (class(data[,col]) == "numeric") {
      data[,col] <- round(data[,col])
    }
  }


  ## then insert the data

  # convert columns to temp columns with quotes around them.
  # otherwise dates will be inserted incorrectly
  tmp_dat <- data
  tmp_dat[,columns] <- lapply(tmp_dat[,columns], shQuote, type="csh")

  rows <- apply(tmp_dat[,columns], 1, paste, collapse=",")
  to_insert <- paste0("(", rows, ")", collapse=",")

  #to_insert <- paste0("(", data$MRN, data$CT1, data$CT2, ")", collapse=",")
  qry2 <- paste0(
    "INSERT INTO #", tbl_name, " (", cols, ") values ",
    to_insert
  )

  sqlQuery(chan, qry2)

  # check if it worked
  chk <- sqlQuery(chan, paste0("select top(5) * from #", tbl_name))
  if (class(chk) == "data.frame") {
    return("success")
  } else {
    return("there was an issue with the table creation")
  }

}
