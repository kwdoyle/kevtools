#' Get counts and percentages from a table split by a grouping parameter
#'
#' !!! This function has been depreciated -- just use MakeCountTables instead
#' Takes an input table and calculates the total and percentages for each variable per group
#' In order to ensure one row per observation, a id_var parameter is required as id_var
#' @param data the input table
#' @param id_var the row-identifier column
#' @param group_var the parameter to split the calculations by
#' @param sum_vars a vector of variables from the table to perform the operation on
#' @export
#' @examples
#' out <- MakeCountTablesGroup(data, id_var="MRN", group_var="Included_in_Study", sum_vars=c("Etiology", "Race", "Gender"))
#' print(out$Etiology)

MakeCountTablesGroup <- function(data, id_var="MRN", group_var="CMD", sum_vars) {
  library(dplyr)

  warning("This function is depreciated. Use MakeCountTables() instead.")

  out_lst <- list()
  for (nm in sum_vars) {
    out <- data %>%
      # add an ungroup here just in case the input data was already grouped by something
      ungroup() %>%
      select_(.dots=c(id_var, group_var, nm)) %>%
      distinct() %>%
      #count_(nm)
      group_by_(group_var, nm) %>%
      mutate(total = n()) %>%
      group_by_(group_var) %>%
      mutate(N = n(),
             pcnt = (total / N) * 100) %>%
      select_(.dots=c(nm, group_var, "total", "N", "pcnt")) %>%
      distinct()

    out_lst[[nm]] <- out

  }

  return(out_lst)
}
