#' Get counts and percentages from a table
#'
#' Takes an input table and calculates the total and percentages for each variable.
#' In order to ensure one row per observation, a id_var parameter is required.
#' @param data the input table
#' @param id_var the row-identifier column
#' @param group_var the parameter to group the counts and percentages by
#' @param sum_vars a vector of variables from the table to perform the operation on
#' @export
#' @examples
#' ## Note: must provide either an id_var or a group_var.
#'
#' # N(%)s with a grouping variable
#' out <- MakeCountTables(data, id_var="MRN", group_var="Included_in_Study", sum_vars=c("Etiology", "Race", "Gender"))
#' print(out$Etiology)
#'
#' # N(%s) without a grouping variable
#' out <- MakeCountTables(data, id_var="MRN", sum_vars=c("Etiology", "Race", "Gender"))
#' print(out$Etiology)

MakeCountTables <- function(data, id_var=NULL, group_var=NULL, rm_na=FALSE, sum_vars) {
  library(dplyr)

  if (is.null(id_var) & is.null(group_var)) {
    stop("Must provide either an id_var or a group_var")
  }

  out_lst <- list()
  for (nm in sum_vars) {
    # will put each name-variable as a separate element in a list to give to select_
    # id_var can be null--it will just be ignored.
    # probably don't want to warn about not providing an id_var here too,
    # since then there'd be two warnings if DichotTest is used without an id_var
    # when it uses this function.
    pickcols <- as.list(c(id_var, group_var, nm))
    if (is.null(group_var)) {
      endpickcols <- as.list(c(nm, "N", "pcnt"))
    } else {
      endpickcols <- as.list(c(nm, group_var, "total", "N", "pcnt"))
    }

    if (rm_na) {
      data <- data[!is.na(data[, nm]), ]
    }

    out <- data %>%
      # add an ungroup here just in case the input data was already grouped by something
      ungroup() %>%
      select_(.dots=pickcols) %>%
      distinct() %>%
      # if an id_var is specified, then group by it before calculating the total n
      {if(!is.null(group_var)) group_by_(., group_var) else .} %>%
      # I think this should be the number of unique id_vars if id_var isn't null?
      {if(!is.null(id_var)) mutate_(.data = ., .dots = list(total = paste0("length(unique(",paste0(id_var),"))"))) else mutate_(.data = ., .dots = list(total = "n()"))} %>%
      #{if(is.null(id_var)) mutate_(.data = ., "total = 'n()'") else .}
      #mutate(total = n()) %>%  
      {if(!is.null(group_var)) group_by_(., group_var, nm) else group_by_(., nm)} %>%
      #group_by_(nm) %>%
      mutate(N = n(),
             pcnt = (N / total) * 100) %>%
      select_(.dots=endpickcols) %>%
      distinct()

    out_lst[[nm]] <- out

  }

  return(out_lst)
}
