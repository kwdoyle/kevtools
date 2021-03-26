#' Test for differences in dichotomized numeric variables
#'
#' Dichotomize a variable by > or <= the median value and then perform Fisher's exact test on the resulting contingency table between a grouping variable.
#' This function was formally named OrdinalVarTest.
#' @param data the input data frame
#' @param id_var The identifying variable per observation (e.g., patient number). Defaults to NULL
#' @param group_var the variable to group by
#' @param tst_vars a vector of variables to dichotomize by their median and test
#' @param GCS_compare if parameter happens to be the Glasgow Coma Scale, then dichotomize by > 7 since that's apparently already an agreed upon split in the literature. Defaults to 7
#' @param rep_meas_sum_func If wind up having multiple observations per id_var, this is the function to summarise the tst_vars per id_var by. Defaults to "median"
#' @param check_n_percents set to TRUE if wish to also obtain a count table with percentages. Defaults to FALSE.
#' @export
#' @examples
#' # if have multiple observations per MRN, then can take the median per patient with rep_meas_sum_func="median"
#' # before doing stats.
#' out <- DichotTest(data=df, id_var="MRN", group_var="Included_in_Study", tst_vars=c("Admission_GCS", "Age", "ICU_Stay_Len"),
#' rep_meas_sum_func="median", check_n_percents=TRUE)
#' # see n(%) tables
#' out$Age$n_percent
#' # see contingency table (basically has same counts from n_percent but without the percent)
#' out$Age$tbl
#' # this table is used for the fisher test
#' out$Age$res

DichotTest <- function(data, id_var=NULL, group_var, tst_vars, GCS_compare=7, rep_meas_sum_func="median", check_n_percents=FALSE) {
  tst_out <- list()

  if (check_n_percents==TRUE & is.null(id_var)) {
    stop("If calculating n_percents, then need to specify an id_var")
  }

  # initialize data
  if (is.null(id_var)) {
    pickcols <- as.list(c(group_var, tst_vars))
    warning("No id variable specified -- assuming one row per observation in the input data after selecting the tst_vars")
  } else {
    # you can apparently include a NULL value in the vector inside of as.list and it will just ignore it,
    # but I'll keep the assigning as two separate things with and without id_var just in case...
    pickcols <- as.list(c(id_var, group_var, tst_vars))
  }

  usedat <- data %>%
    ungroup() %>%
    select_(.dots=pickcols) %>%
    distinct() %>%
    ungroup()

  # If have multiple observations per id variable (e.g., patient), then first summarize per id_var.
  if (!is.null(id_var)) {
    message(paste0("If multiple observations per ", id_var, ", will calculate ", rep_meas_sum_func, " per each tst_var per ", id_var))

    usedat2 <- usedat %>%
      group_by_(.dots=c(id_var, group_var)) %>%
      summarise_all(.funs=rep_meas_sum_func) %>%
      # ungrouping here is very important, otherwise the following calculations will still be done per group!
      ungroup()
  }


  for (var in tst_vars) {
    func_name1 <- paste0("median(", var, ", na.rm=T)")
    func_name2 <- paste0("quantile(", var, ", na.rm=T)[2]")
    func_name3 <- paste0("quantile(", var, ", na.rm=T)[4]")
    new_col1 <- paste0("med_", var)
    new_col2 <- paste0("qt25_", var)
    new_col3 <- paste0("qt75_", var)

    # other functions to run after the above.
    # If var == Admission_GCS, then split values where > 7
    if (var == "Admission_GCS") {
      func_name4 <- paste0("if_else(", var, ">=", GCS_compare, ", true=1, false=0)")

    } else {
      func_name4 <- paste0("if_else(", var, ">=", new_col1, ", true=1, false=0)")
    }

    new_col4 <- paste0("dichot_", var)
    # vvv the comments below are from my revelation about how .dots can work.

    # OMG ALL YOU HAVE TO DO IS MAKE A LIST OF FUNCTIONS AS STRINGS
    list_of_func <- list(
      func_name1,
      func_name2,
      func_name3,
      func_name4
    )

    # AND THEN NAME THEM AS WHATEVER YOU WANT THE NEW COLUMN FROM THE FUNCTION TO BE CALLED
    names(list_of_func) <- c(new_col1, new_col2, new_col3, new_col4)

    newdata <- usedat %>%
      # Can now perform overall summary stats after potentially dealing with multiple observations per id_var
      # THIS DOES WORK
      mutate_(.dots=list_of_func)  # I don't think I even need the IQR at this step.

    # can use xtabs to make the contingency table
    form <- as.formula(paste0("~", group_var, "+", new_col4))

    tbl <- xtabs(form, data=newdata)
    # then do fisher test
    res <- fisher.test(tbl)

    # compare value
    if (var == "Admission_GCS") {
      compare_val <- GCS_compare
    } else {
      compare_val <- unique(newdata[,new_col1])  # unique(pull(newdata[,new_col1]))
    }


    # N(%s)s
    if (check_n_percents == TRUE) {
      # uses the new column name saved as new_col4 as the new summary variable to use in MakeCountTablesGroup.
      n_pcnt <- MakeCountTables(newdata, id_var=id_var, group_var=group_var, sum_vars=new_col4)

      tst_out[[var]] <- list(rawdat=newdata, tbl=tbl, res=res, compare_val=compare_val, n_percent=n_pcnt[[new_col4]])

    } else {
      tst_out[[var]] <- list(rawdat=newdata, tbl=tbl, res=res)
    }


  }

  return(tst_out)

}
