#' Multiple linear or logistinc regression
#'
#' Perform a multiple linear or logistic regression (i.e., one with multiple dependent variables)
#' @param table the input data
#' @param depen_vars a vector of dependent variables
#' @param indep_vars a vector of independent variables
#' @param modtype The type of model. Can either be "lm" or "glm"
#' @export
#' @examples
#' out <- MultiRegress(table, depen_vars=c("mRS_Discharge", "mRS_3Month", "TICS_3Month"),
#'                            indep_vars=c("Total.EEG.Time", "Suppression", "Burst.Suppression"), modtype="glm")

MultiRegress <- function(table, depen_vars, indep_vars, modtype="lm") {
  ## depen_vars must be a vector of length==1 of the variable names
  ## (e.g., c("mRS_3Month, TICS_3Month")),
  ## while indep_vars must be a vector of length==n, where n==number of independent variables
  ## (e.g., c("Total.EEG.Time", "Epil.Disch", "Triphasic", etc.))

  dep <- paste(depen_vars)

  response <- paste("cbind(",dep,") ~ ", sep="")
  effect <- paste(indep_vars, collapse="+")
  datanm <- paste(", data = ", deparse(substitute(table)), sep="")

  formula <- paste(response, effect, sep="")  # , datanm
  form <- as.formula(formula)

  if (modtype == "lm") {
    fit <- lm(form, data = table)
  }
  else if (modtype == "glm") {
    fit <- glm(form, data = table, family = binomial)
  }
  else {
    stop("must specify either 'lm' or 'glm' for modtype")
  }


  return(fit)
}
