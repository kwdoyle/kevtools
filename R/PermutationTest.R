#' Run a permutation test
#'
#' Old R function from Biometry that performs a permutation test on a dataset, first split into two by the grouping variable
#' because THAT'S the most efficient way to do this.
#' @param group1dat data from group 1
#' @param group2dat data from group 2
#' @param num_permutations the number of permutations required
#' @param test_type the desired test statistic. defaults to mean.
#' @export
#' @examples
#' out <- PermutationTest(group1dat=MaleData, group2dat=FemaleData, num_permutations=500, test_type="mean")

PermutationTest <- function(group1dat, group2dat, num_permutations, test_type="mean") {


  # combine the two datasets into a single dataset
  # i.e., under the null hypothesis, there is no difference in glucose levels between
  # the two groups, which will then have the glucose levels randomly allocated to males
  # and females inside the for loop
  combined <- c(group1dat, group2dat)

  # observed difference in means
  if (test_type == "mean") {
    diff.observed <- mean(group1dat, na.rm=T) - mean(group2dat, na.rm=T)
  } else if (test_type == "median") {
    diff.observed <- median(group1dat, na.rm=T) - median(group2dat, na.rm=T)
  } else {
    stop("test_type needs to either be 'mean' or 'median'")
  }


  #num_permutations <- 10000  # 10,000 seems to always give p = 0.01

  diff.random <- NULL
  for (i in 1:num_permutations)
  {
    # sample from the combined dataset without replacement
    # i.e., create a randomized assignment of gender to glucose level
    shuffled <- sample(combined, length(combined))

    # assign randomized values to the set number of observations in each group
    group1dat.random <- shuffled[1:length(group1dat)]
    group2dat.random <- shuffled[(length(group1dat) + 1):length(combined)]  # continues adding from where group1dat.random stopped

    # null (permuated) difference
    if (test_type == "mean") {
      diff.random[i] <- mean(group1dat.random, na.rm=T) - mean(group2dat.random, na.rm=T)
    } else if (test_type == "median") {
      diff.random[i] <- median(group1dat.random, na.rm=T) - median(group2dat.random, na.rm=T)
    }

  }

  # p-value is the fraction of how many times the permuted difference in means is equal
  # or more extreme than the observed difference in means
  # this calculation of being equal or more extreme is also stated in the
  # Randomization tests.pdf of the scanned textbook

  pvalue <- sum(abs(diff.random) >= abs(diff.observed)) / num_permutations

  return(pvalue)

}
