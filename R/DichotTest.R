#' Test for differences in dichotomized numeric variables
#'
#' Dichotomize a variable by > or <= the median value and then perform Fisher's exact test on the resulting contingency table between a grouping variable.
#' This function was formally named OrdinalVarTest.
#' @param data the input data frame
#' @param group_var the variable to group by
#' @param tst_vars a vector of variables to dichotomize by their median and test
#' @param GCS_compare if parameter happens to be the Glasgow Coma Scale, then dichotomize by > 7 since that's apparently already an agreed upon split in the literature. Defaults to 7
#' @param check_n_percents set to TRUE if wish to also obtain a count table with percentages. Defaults to FALSE.
#' @export
#' @examples
#' out <- DichotTest(data=df, group_var="Included_in_Study", tst_vars=c("Admission_GCS", "Age", "ICU_Stay_Len"), check_n_percents=TRUE)
#' # see n(%) tables
#' out$Age$n_percent

DichotTest <- function(data, group_var, tst_vars, GCS_compare=7, check_n_percents=FALSE) {
  tst_out <- list()
  # initialize data
  usedat <- data %>%
    select_(.dots=c("MRN", group_var, tst_vars)) %>%
    distinct() %>%
    ungroup()

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
      func_name4 <- paste0("if_else(", var, ">", GCS_compare, ", true=1, false=0)")

    } else {
      func_name4 <- paste0("if_else(", var, ">", new_col1, ", true=1, false=0)")
    }

    new_col4 <- paste0("dichot_", var)

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
      # get one value per patient first
      # SHOULD ALSO BE PER RECORDING
      ## shouldn't split by median per group--should be per overall median
      #group_by_(group_var) %>%
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
      compare_val <- unique(pull(newdata[,new_col1]))
    }


    # N(%s)s
    if (check_n_percents == TRUE) {
      n_pcnt <- MakeCountTables2(newdata, group_var1="MRN", group_var2=group_var, sum_vars=new_col4)

      tst_out[[var]] <- list(rawdat=newdata, tbl=tbl, res=res, compare_val=compare_val, n_percent=n_pcnt[[new_col4]])

    } else {
      tst_out[[var]] <- list(rawdat=newdata, tbl=tbl, res=res)
    }


  }

  return(tst_out)

}
