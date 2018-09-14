#' Perform a univariate regression analysis
#'
#' Will perform a logistic regression for each independent vatiable in the input table and output the results
#' in a data frame
#' @param table the input data
#' @param depen_vars the dependent variables
#' @param indep_vars the independent variables
#' @param mod_type Type of model to run. Must either be "glm" or "glmer"
#' @param rand_effect If mod_type is "glmer", pass whichever variable you want as the random effect as a string. Defaults to NULL.
#' @export
#' @examples
#' out <- UnivariateRegress(table, depen_vars="Alive_Dch", indep_vars=c("Total.EEG.Time", "Supression", "Beta.Burst"))

UnivariateRegress <- function (table, depen_vars, indep_vars, mod_type="glm", rand_effect=NULL) {
  output_table <- data.frame()
  dep <- paste(depen_vars)
  response <- paste("cbind(", dep, ") ~ ", sep = "")
  for (i in 1:length(indep_vars)) {


    if (mod_type == "glm") {
      effect <- paste(indep_vars[i])
      formula <- paste(response, effect, sep = "")
      form <- as.formula(formula)
      fit <- try(glm(form, family = binomial, data = table),
                 silent = F)
      if (any(class(fit) == "try-error")) {
        message(paste("variable", indep_vars[i], " has error"))
        return(NA)
      }

      smry <- summary(fit)
      params <- try(matrix(smry$coefficients[2, ], nrow = 1,
                           ncol = length(smry$coefficients[2, ]), byrow = T),
                    silent = T)
      if (class(params) != "try-error") {
        params <- matrix(c(params, c(confint(fit)[2, ])),
                         nrow = 1)
        colnames(params) <- c(colnames(smry$coefficients),
                              paste("CI", colnames(confint(fit))))
        rownames(params) <- rownames(smry$coefficients)[2]
        output_table <- rbind(output_table, as.data.frame(params))
      } else {
        params <- matrix(c(rep(NA, length(output_table))),
                         nrow = 1)
        colnames(params) <- colnames(output_table)
        rownames(params) <- effect
        output_table <- rbind(output_table, as.data.frame(params))
      }



    } else if (mod_type == "glmer") {
      library(lme4)

      effect <- paste0(indep_vars[i], " + (1 | ", rand_effect, ")" )
      formula <- paste(response, effect, sep = "")
      form <- as.formula(formula)
      fit <- try(glmer(form, family = binomial, data = table),
                 silent = F)
      if (any(class(fit) == "try-error")) {
        message(paste("variable", indep_vars[i], " has error"))
        return(NA)
      }

      smry <- summary(fit)
      params <- try(matrix(smry$coefficients[2, ], nrow = 1,
                           ncol = length(smry$coefficients[2, ]), byrow = T),
                    silent = T)
      # here's some crazy fix for if the confint calc fails.
      if (class(params) != "try-error") {
        oth_params <- try(c(confint.merMod(fit)[2, ]), silent=T)

        if (class(oth_params) == "try-error") {
          oth_params <- c(NA, NA)
          params <- matrix(c(params, oth_params), nrow = 1)

          colnames(params) <- c(colnames(smry$coefficients),
                                c("CI 2.5 %", "CI 97.5 %"))

        } else {
          params <- matrix(c(params, oth_params), nrow = 1)

          colnames(params) <- c(colnames(smry$coefficients),
                                paste("CI", colnames(confint.merMod(fit))))
        }


        rownames(params) <- rownames(smry$coefficients)[2]
        output_table <- rbind(output_table, as.data.frame(params))

      } else {
        params <- matrix(c(rep(NA, length(output_table))),
                         nrow = 1)
        colnames(params) <- colnames(output_table)
        rownames(params) <- indep_vars[i]
        output_table <- rbind(output_table, as.data.frame(params))
      }

    }


  }
  output_table$`Pr(>|z|)` <- round(output_table$`Pr(>|z|)`,
                                   digits = 3)
  output_table$`Pr(>|z|)` <- ifelse(output_table$`Pr(>|z|)` <
                                      0.05, yes = paste(output_table$`Pr(>|z|)`, "*"), no = output_table$`Pr(>|z|)`)
  return(output_table)
}
