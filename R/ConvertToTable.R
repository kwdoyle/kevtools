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

ConvertToTable <- function(obj, flip_dir=FALSE) {
  output_table <- data.frame()
  # smry <- summary(fit)
  if ('glm' %in% class(obj) | 'lm' %in% class(obj)) {
    fit <- obj
  } else {
    fit <- obj$res
  }

  smry <- summary(fit)
  if (flip_dir) {
    var_name <- as.character(fit$formula[2])
  } else {
    var_name <- as.character(fit$formula[3])
  }

  params <- try(matrix(smry$coefficients[2, ], nrow = 1,
                       ncol = length(smry$coefficients[2, ]), byrow = T),
                silent = T)

  if (!'try-error' %in% class(params)) {
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
