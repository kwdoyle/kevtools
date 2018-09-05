#' Compare qualitative variables between a grouping variable
#'
#' Make contingency table of counts per group and, if the table is 2x2, perform Fisher's exact test. If not, perform a chi square test.
#' Can perform tests counts of each value within a variable as well if multilevel is set to TRUE.
#' @param data the input data frame
#' @param group_var the grouping variable
#' @param tst_vars variables to perform the test on
#' @param multilevel if TRUE, then function will perform analyses on all values with a variable between the group
#' @export
#' @examples
#' out <- QualitativeStatistics(data, group_var="CMD", tst_vars=c("Gender", "Etiology", "Race"), multilevel=TRUE)
#' # since multilevel is TRUE, will have results for male and female, all etiologies, and all races.
#' print(out$Etiology$SAH)
#' print(out$Etiology$ICH)
#' print(out$Gender$M)
#' print(out$Gender$F)

QualitativeStatistics <- function(data, group_var, tst_vars, multilevel=TRUE) {
  library(dplyr)
  #library(rlang) # not sure if I need rlang in here anymore
  chi_out <- list()

  # make sure data is not a tibble so that some of the below opperations work
  data <- as.data.frame(data)

  if (multilevel==TRUE) {

    for (var in tst_vars) {
      sub_chi_out <- list()
      # want to analyze differences between each category per sum_var column between CMD+ and CMD-
      sub_vars <- unique(data[,var])

      for (sub_var in sub_vars) {
        func_name <- paste0("case_when(", var, " == '", sub_var, "' ~ ", var, ", TRUE ~ 'not", sub_var, "')")
        new_col <- paste0("not", sub_var) # should probably name this as: paste0("new", var).
        # Eh, never mind. this unnecessarily breaks everything and it'll be a pain to fix.

        datuse <- data %>%
          ungroup() %>%
          select_(.dots=c("MRN", group_var, var)) %>%
          distinct() %>%
          # remove any rows where the var of interest is missing
          # to prevent xtabs from making a new column for missing values
          filter_(paste0("!is.na(", var, ")")) %>%
          # instead of filtering for only the sub_var of interest,
          # to include those who are NOT the sub_var, then can
          # case_when mutate any names that aren't the sub_var to
          # something like "not sub_var".
          mutate_(.dots=setNames(func_name, new_col))

        # I think the new column needs to be turned into a factor and always have the 'notX' level first
        datuse[,new_col] <- factor(datuse[,new_col], levels=c(new_col, sub_var))
        # ..or maybe using xtabs fixes this now? idk, it all currently works. no need to change anything..

        #tabl <- table(datuse[,group_var], datuse[,new_col]) #datuse[,var])
        # try making a table with xtabs instead so I keep the names of each column/row
        # have to include the new_col name in ticks `` in case the name is weird and has slashes and minuses in it.
        form <- as.formula(paste0("~", group_var, "+ `", new_col, "`"))

        tabl <- xtabs(form, data=datuse)
        # do a fisher test if the table is 2x2
        if (all(dim(tabl) == c(2,2))) {
          chi_res <- fisher.test(tabl)
        } else {
          chi_res <- chisq.test(tabl)
        }

        sub_chi_out[[sub_var]] <- list(tbl=tabl, res=chi_res)

        chi_out[[var]] <- sub_chi_out


      }

    }

  } else {

    for (var in tst_vars) {
      #### I don't think this part is going to be used anyway, since we can't/shouldn't do a full-on
      #### chisquare test on these ordinal scale variables that have ~ 15 levels.

      # set names for summary table
      # ...ok can't take median for these because it can create new values, e.g., decimal values in-between the actual values.
      # just take the max value instead..?
      func_name <- paste0("max(", var, ", na.rm=T)")
      new_col <- var #paste0("max_", var) # just rename it the same as the original

      datuse <- data %>%
        ungroup() %>%
        select_(.dots=c("MRN", group_var, var)) %>%
        distinct() %>%
        # remove any rows where the var of interest is missing
        # to prevent xtabs from making a new column for missing values
        filter_(paste0("!is.na(", var, ")")) %>%
        # ok, so if a patient has multiple values per variable,
        # then I guess take the median value per patient?
        group_by(MRN, CMD) %>%
        #summarise_(med_var = median(var, na.rm=T))
        summarise_(.dots=setNames(func_name, new_col)) %>%
        # set as data frame so can pull columns as vectors below
        as.data.frame()



      #tabl <- table(datuse[,group_var], datuse[,new_col]) #datuse[,var])
      # try making a table with xtabs instead so I keep the names of each column/row
      form <- as.formula(paste0("~", group_var, "+ `", new_col, "`"))

      tabl <- xtabs(form, data=datuse)
      # do a fisher test if the table is 2x2
      if (all(dim(tabl) == c(2,2))) {
        chi_res <- fisher.test(tabl)
      } else {
        chi_res <- chisq.test(tabl)
      }

      chi_out[[var]] <- list(tbl=tabl, res=chi_res)


    }


  }

  return(chi_out)

}