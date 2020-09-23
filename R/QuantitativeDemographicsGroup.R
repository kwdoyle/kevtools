#' Summary statistics on quantitative variables per group
#'
#' Calculates mean, st.dev, median, IQR, min, and max on a vector of variables per group
#' @param data the input data
#' @param id_var the row identifier variable--ensures one row per observation
#' @param group_var the grouping variable
#' @param sum_vars the variables to perform calculations on
#' @export
#' @examples
#' out <- QuantitativeDemographicsGroup(data, id_var="MRN", group_var="CMD", sum_vars=c("Age", "Admission_GCS", "GOSE"))
#' print(out$Age)

# second version for grouping by two variables
QuantitativeDemographicsGroup <- function(data, id_var="MRN", group_var="CMD", sum_vars) {
  library(dplyr)

  out_lst <- list()
  for (nm in sum_vars) {
    out <- data %>%
      ungroup() %>%
      select_(.dots=c(id_var, group_var, nm)) %>%
      distinct()

    # check to make sure the number of rows in this table
    # is equal to the number of unique values of the group_var.
    # otherwise this would mean there are duplicate values/
    # one patient has multiple values
    # if (nrow(out) != length(unique(pull(out, group_var)))) {
    #   warning(paste("Duplicate values found per", group_var, "for", nm), noBreaks.=TRUE)
    # }

    out <- out %>%
      # need to remove any instances of Inf or -Inf caused from calculating hospital/ICU stay lengths when some are missing
      filter_(paste0("is.finite(", nm, ")")) %>%
      group_by_at(., vars(one_of(group_var))) %>%
      summarise_(avg = paste("mean(", nm, ", na.rm=T)", sep=""),
                 sd = paste("sd(", nm, ", na.rm=T)", sep=""),
                 med = paste("median(", nm, ", na.rm=T)", sep=""),
                 qt_25 = paste("quantile(", nm, ", na.rm=T)[2]", sep=""),
                 qt_75 = paste("quantile(", nm, ", na.rm=T)[4]", sep=""),
                 min = paste("min(", nm, ", na.rm=T)", sep=""),
                 max = paste("max(", nm, ", na.rm=T)", sep=""))

    out_lst[[nm]] <- out
  }

  return(out_lst)

}
