#' Get counts and percentages from a table
#'
#' Takes an input table and calculates the total and percentages for each variable.
#' In order to ensure one row per observation, a id_var parameter is required.
#' @param data the input table
#' @param id_var the row-identifier column
#' @param sum_vars a vector of variables from the table to perform the operation on
#' @export
#' @examples
#' out <- MakeCountTables(data, id_var="MRN", sum_vars=c("Etiology", "Race", "Gender"))
#' print(out$Etiology)

MakeCountTables <- function(data, id_var, sum_vars) {
  library(dplyr)

  out_lst <- list()
  for (nm in sum_vars) {
    out <- data %>%
      # add an ungroup here just in case the input data was already grouped by something
      ungroup() %>%
      select_(.dots=c(id_var, nm)) %>%
      distinct() %>%
      mutate(total = n()) %>%
      group_by_(nm) %>%
      mutate(N = n(),
             pcnt = (N / total) * 100) %>%
      select_(.dots=c(nm, "N", "pcnt")) %>%
      distinct()

    out_lst[[nm]] <- out

  }

  return(out_lst)
}
