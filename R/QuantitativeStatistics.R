#' Comparative stats on quantitative variables per group
#'
#' Performs either a t-test or wilcox test on quantitative variables per group.
#' This requires that "MRN" is a column already present in the input data. If this is used in the future,
#' I might want to change that.
#' @param data the input data
#' @param group_var the variable to group by
#' @param tst_vars varaibles to analyze
#' @param test_type The desired test. Can be either "t.test" or "wilcox.test". Defaults to "t.test"
#' @export
#' @examples
#' out <- QuantitativeStatistics(data, group_var="CMD", tst_vars=c("Age", "Hosp_Stay_Len"), test_type="wilcox.test")

QuantitativeStatistics <- function(data, group_var, tst_vars, test_type="t.test") {
  library(dplyr)
  #library(rlang) # not sure if I need rlang in here anymore
  test_out <- list()
  # make sure data is not a tibble so that some of the below opperations work
  data <- as.data.frame(data)

  for (var in tst_vars) {

    datuse <- data %>%
      ungroup() %>%
      select_(.dots=c("MRN", group_var, var)) %>%
      distinct() %>%
      # convert var to numeric just in case it's in some other format (like difftime.)
      mutate_(.dots=setNames(paste0("as.numeric(", var, ")"), var)) %>%
      # since the above distinct with including MRN gaurantees
      # that there'll be one row per patient, I can then just unselect
      # MRN and have only the column of interest and CMD.
      select(-MRN)

    # maybe put in a check just to make sure the number of rows in this table
    # matches the number of unique patients..
    if (length(unique(data$MRN)) != nrow(datuse)) {
      warning(paste("Some patients have multiple values for", var))
    }

    # make formula for t test
    form <- as.formula(paste(rev(names(datuse)), collapse="~"))
    if (test_type == "t.test") {
      res <- t.test(form, data=datuse)
    } else if (test_type == "wilcox.test") {
      res <- wilcox.test(form, data=datuse)
    }


    test_out[[var]] <- res

  }

  return(test_out)

}
