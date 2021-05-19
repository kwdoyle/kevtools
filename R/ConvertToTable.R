#' Convert list-form model outputs into a table
#'
#' @param obj any list-output from one of the many functions in this package that generates model outputs
#' @export
#' @examples
#' out <- DichotTest(data)
#' table_out <- bind_rows(lapply(out, ConvertToTable))
#' # this converts the coefficients into odds ratios
#' table_out[,c(1,5,6)] <- lapply(table_out[,c(1,5,6)], exp)
#'
#' # if running tests where multilevel == TRUE, table formatting should be done like this:
#' multilevel_stess_correct <- QualitativeStatistics(data=merged_dat, id_var='MRN', group_var=ANALYZE_VAR,
#'                                                   tst_vars=c('n_pressors', 'EEG'), multilevel=T,
#'                                                   test_use='logistic_regress', correct_var='STESS_score')
#'
#' multilevel_out_stess_lst <- lapply(multilevel_stess_correct, function(x) lapply(x, function(y) ConvertToTable(y)))
#' multilevel_out_stess_dfs <- lapply(multilevel_out_stess_lst, bind_rows)
#' multilevel_out_stess_dfs <- lapply(multilevel_out_stess_dfs, function(x) {
#'   x[,c(1,5,6)] <- lapply(x[,c(1,5,6)], exp)
#'   return(x)
#' })

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
    rownames(params) <- var_name
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
