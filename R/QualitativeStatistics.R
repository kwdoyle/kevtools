#' Compare qualitative variables between a grouping variable
#'
#' Make contingency table of counts per group and, if the table is 2x2, perform Fisher's exact test. If not, perform a chi square test.
#' Can perform tests counts of each value within a variable as well if multilevel is set to TRUE.
#' @param data the input data frame
#' @param id_var the id variable to group obsetrvations by
#' @param group_var the grouping variable, passed as a string
#' @param tst_vars variables to perform the test on; can be passed as a vector of strings
#' @param multilevel if TRUE, then function will perform analyses on all values with a variable between the group
#' @param test_use specify either 'proportion' or 'logistic_regress' Note: logistic regression is only done if multilevel == TRUE
#' @param correct_var if performing a logistic regression, can specify an additional variable to correct for
#' @param flip_dir short for 'flip directionality.' set to true if want to predict the variable *using* the outcome instead
#' @export
#' @examples
#' out <- QualitativeStatistics(data, group_var = "CMD", tst_vars = c("Gender", "Etiology", "Race"), multilevel = TRUE)
#' # since multilevel is TRUE, will have results for male and female, all etiologies, and all races.
#' print(out$Etiology$SAH)
#' print(out$Etiology$ICH)
#' print(out$Gender$M)
#' print(out$Gender$F)


QualitativeStatistics <- function(data, id_var, group_var, tst_vars, multilevel = FALSE,
                                  test_use='proportion', flip_dir = FALSE, correct_var=NULL) {
  library(dplyr)
  chi_out <- list()
  # make sure data is not a tibble so that some of the below opperations work
  data <- as.data.frame(data)

  if (multilevel == TRUE) {

    for (var in tst_vars) {
      sub_chi_out <- list()
      # I think NAs should be removed here, becase after removing them below,
      # there will only be one value in the new column of "not 'sub_var'" which would be 'notNA'
      sub_vars <- unique(data[!is.na(data[, var]), var])

      for (sub_var in sub_vars) {
        # if the variable is a factor, then this will fail because it will try to re-assign the factor if TRUE,
        # else assign a character ('notvar') if false.
        # always set to check the variable as a character.
        func_name <- paste0("case_when(as.character(", var, ") == '", sub_var, "' ~ as.character(", var, "), TRUE ~ 'not", sub_var, "')")
        # ...do I actually need it to say 'not X'..?
        # new_col <- paste0("not", sub_var) # should probably name this as: paste0("new", var).
        new_col <- paste0('var_', sub_var)
        # Eh, never mind. this unnecessarily breaks everything and it'll be a pain to fix.

        datuse <- data %>%
          ungroup() %>%
          select_(.dots = c(id_var, group_var, var, correct_var)) %>%
          distinct() %>%
          # remove any rows where the var of interest is missing
          # to prevent xtabs from making a new column for missing values
          filter_(paste0("!is.na(", var, ")")) %>%
          # instead of filtering for only the sub_var of interest,
          # to include those who are NOT the sub_var, then can
          # case_when mutate any names that aren't the sub_var to
          # something like "not sub_var".
          mutate_(.dots = setNames(func_name, new_col))

        # I think the new column needs to be turned into a factor and always have the 'notX' level first
        # use the 'not X' here for the levels
        # datuse[, new_col] <- factor(datuse[, new_col], levels = c(paste0('not', new_col), as.character(sub_var)))
        datuse[, new_col] <- factor(datuse[, new_col], levels = c(paste0('not', sub_var), as.character(sub_var)))
        # ..or maybe using xtabs fixes this now? idk, it all currently works. no need to change anything..

        #tabl <- table(datuse[,group_var], datuse[,new_col]) #datuse[,var])
        # try making a table with xtabs instead so I keep the names of each column/row
        # have to include the new_col name in ticks `` in case the name is weird and has slashes and minuses in it.
        if (test_use == 'proportion') {
          form <- as.formula(paste0("~", group_var, "+ `", new_col, "`"))

          tabl <- xtabs(form, data = datuse)
          # do a fisher test if the table is 2x2
          if (all(dim(tabl) == c(2,2))) {
            res <- fisher.test(tabl)
          } else {
            res <- chisq.test(tabl)
          }

        } else if (test_use == 'logistic_regress') {
          tabl <- NULL
          if (is.null(correct_var)) {
            # TODO: NOTE: This is set to PREDICT each variable USING
            # the group var. We probably are going to want to flip this for later projects.
            # ALSO for this, need to make "variable/NOT-variable" as one column, not two.
            # Avoid non-standard-eval dplyr stuff by just editing the second-to-last column.
            # datuse[,ncol(datuse)-1] <- as.factor(ifelse(!is.na(datuse[,ncol(datuse)]),
            #                                             yes=as.character(datuse[,ncol(datuse)]),
            #                                             no=as.character(datuse[,ncol(datuse)-1])))
            # wait, the 'new column' is already supposed to be in this format.

            # This should fix the above snafu.
            if (flip_dir) {
              dep_var <- new_col
              indep_var <- group_var
            } else {
              dep_var <- group_var
              indep_var <- new_col
            }


            form <- as.formula(paste0(dep_var, "~", indep_var))
          } else {
            form <- as.formula(paste0(dep_var, "~", indep_var, "+", correct_var))
          }

          res <- glm(form, family='binomial', data=datuse)

        }


        # make sure the name of the new list element is a character. using numbers can cause problems
        sub_chi_out[[as.character(sub_var)]] <- list(tbl = tabl, res = res)

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

      # convert column to character before continuing, else that 'selecting
      # max to remove duplicates' breaks.
      data[,var] <- as.character(data[,var])

      datuse <- data %>%
        ungroup() %>%
        select_(.dots = c(id_var, group_var, var, correct_var)) %>%
        distinct() %>%
        # remove any rows where the var of interest is missing
        # to prevent xtabs from making a new column for missing values
        filter_(paste0("!is.na(", var, ")")) %>%
        # ok, so if a patient has multiple values per variable,
        # then I guess take the median value per patient?
        group_by_(.dots = c(id_var, group_var, correct_var)) %>%
        #summarise_(med_var = median(var, na.rm=T))
        summarise_(.dots = setNames(func_name, new_col)) %>%
        # set as data frame so can pull columns as vectors below
        as.data.frame()


      #tabl <- table(datuse[,group_var], datuse[,new_col]) #datuse[,var])
      # try making a table with xtabs instead so I keep the names of each column/row
      if (test_use == 'proportion') {
        form <- as.formula(paste0("~", group_var, "+ `", new_col, "`"))

        tabl <- xtabs(form, data = datuse)
        # do a fisher test if the table is 2x2
        if (all(dim(tabl) == c(2, 2))) {
          res <- fisher.test(tabl)
        } else {
          res <- chisq.test(tabl)
        }

      } else if (test_use == 'logistic_regress') {
        tabl <- NULL
        if (is.null(correct_var)) {
          # TODO: NOTE: This is set to PREDICT each variable USING
          # the group var. We probably are going to want to flip this for later projects.
          # ALSO for this, need to make "variable/NOT-variable" as one column, not two.
          # Avoid non-standard-eval dplyr stuff by just editing the second-to-last column.
          # datuse[,ncol(datuse)-1] <- as.factor(ifelse(!is.na(datuse[,ncol(datuse)]),
          #                                             yes=as.character(datuse[,ncol(datuse)]),
          #                                             no=as.character(datuse[,ncol(datuse)-1])))
          # wait, the 'new column' is already supposed to be in this format.

          # This should fix the above snafu.
          if (flip_dir) {
            dep_var <- new_col
            indep_var <- group_var
          } else {
            dep_var <- group_var
            indep_var <- new_col
          }

          form <- as.formula(paste0('as.factor(', dep_var, ')', "~", indep_var))
        } else {
          form <- as.formula(paste0('as.factor(', dep_var, ')', "~", indep_var, "+", correct_var))
        }

        res <- glm(form, family='binomial', data=datuse)

      }

      chi_out[[var]] <- list(tbl = tabl, res = res)

    }

  }

  return(chi_out)

}
