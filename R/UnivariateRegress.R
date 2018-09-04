#' Perform a univariate regression analysis
#'
#' Will perform a logistic regression for each independent vatiable in the input table and output the results
#' in a data frame
#' @param table the input data
#' @param depen_vars the dependent variables
#' @param indep_vars the independent variables
#' @export
#' @examples
#' out <- UnivariateRegress(table, depen_vars="Alive_Dch", indep_vars=c("Total.EEG.Time", "Supression", "Beta.Burst"))

UnivariateRegress <- function(table, depen_vars, indep_vars) {

  output_table <- data.frame()

  dep <- paste(depen_vars)
  response <- paste("cbind(",dep,") ~ ", sep="")

  for (i in 1:length(indep_vars)) {

    # see if cbinding a single variable is the same as supplying just the variable. I think it is

    effect <- paste(indep_vars[i])

    formula <- paste(response, effect, sep="")
    form <- as.formula(formula)

    fit <- try(glm(form, family=binomial, data=table), silent=F)
    if (any(class(fit) == "try-error")) {
      message(paste("variable", indep_vars[i]," has error"))
      return(NA)
    }
    smry <- summary(fit)

    params <- try(matrix(smry$coefficients[2,], nrow=1, ncol=length(smry$coefficients[2,]), byrow=T), silent=T)
    # add the confidence intervals, but first check if model was able to be fit for this independent variable.
    if (class(params) != "try-error") {
      params <- matrix(c(params, c(confint(fit)[2,])), nrow=1)
      colnames(params) <- c(colnames(smry$coefficients), paste("CI", colnames(confint(fit))))
      rownames(params) <- rownames(smry$coefficients)[2]

      output_table <- rbind(output_table, as.data.frame(params))
    } else {
      params <- matrix(c(rep(NA, length(output_table))), nrow=1)
      # I think this will break if the very first row to be added has NAs/is a try-error, since then there'll be no colnames/rownames to choose from.
      colnames(params) <- colnames(output_table)
      rownames(params) <- effect
      output_table <- rbind(output_table, as.data.frame(params))
    }


  }

  # mark p < 0.05 w/ star
  output_table$`Pr(>|z|)` <-  round(output_table$`Pr(>|z|)`, digits=3)
  output_table$`Pr(>|z|)` <-  ifelse(output_table$`Pr(>|z|)` < 0.05, yes=paste(output_table$`Pr(>|z|)`, "*"), no=output_table$`Pr(>|z|)`)


  return(output_table)



}
