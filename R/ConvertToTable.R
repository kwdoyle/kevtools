# early form of function that can take outputs from other functions
# (eg, DichotTest or QualitativeStatistics) and put the results all in a single table

ConvertToTable <- function(obj) {
  output_table <- data.frame()
  # smry <- summary(fit)
  if ('glm' %in% class(obj) | 'lm' %in% class(obj)) {
    fit <- obj
  } else {
    fit <- obj$res
  }

  smry <- summary(fit)
  var_name <- as.character(fit$formula[2])
  # this always takes the second row of the output
  # what does it take with the multivariate?
  params <- try(matrix(smry$coefficients[2, ], nrow = 1,
                       ncol = length(smry$coefficients[2, ]), byrow = T),
                silent = T)

  if (class(params) != "try-error") {
    params <- matrix(c(params, c(confint(fit)[2, ])),
                     nrow = 1)
    colnames(params) <- c(colnames(smry$coefficients),
                          paste("CI", colnames(confint(fit))))
    rownames(params) <- var_name  # rownames(smry$coefficients)[2]
    output_table <- rbind(output_table, as.data.frame(params))
  } else {
    params <- matrix(c(rep(NA, length(output_table))),
                     nrow = 1)
    colnames(params) <- colnames(output_table)
    rownames(params) <- effect
    output_table <- rbind(output_table, as.data.frame(params))
  }

  return(output_table)
}


#ok <- ConvertToTable(fit=)

# hmm
#lapply(multilevel_stess_correct, function(x) lapply(x, function(y) ConvertToTable(y)) )
#ok <- lapply(multilevel_stess_correct, ConvertToTable)

# ok <- lapply(LOS_dichot, ConvertToTable)

