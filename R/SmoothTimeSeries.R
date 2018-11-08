#' Clean a time series
#'
#' Smooth out time series data using a median and then a mean filter over specified windows of time.
#' @param data the input data frame
#' @param id_col name of column, passed as a string, to identify observations by, e.g., a patient identifier
#' @param param_col name of column, as string, specifying names of parameters to perform smoothing for.
#' @param var_col string name of column containing the values for the parameters specified in param_col
#' @param time_col the column containing the timestamps passed as a string
#' @param median_mins window length of data in minutes to perform the median filtering on
#' @param mean_mins window length of data in minutes to perform the mean filtering on
#' @param n_sec_per_row Length of time in seconds that each row of data corresponds to. Defaults to 5 for BedMaster data.
#' @param k1 number of rows to perform median filter on. Only used if the same amount of time does not pass between each row and median_mins is not specified
#' @param k2 number of rows to perform mean filter on. Only used if the same amount of time does not pass between each row and mean_mins is not specified
#' @export
#' @examples
#' median_mins = 3
#' mean_mins = 5
#'
#' tstdat2 <- SmoothTimeSeries(data = tstdat,
#'                             id_col = "PATIENT_ID",
#'                             param_col = "ParName",
#'                             var_col = "ParValue",
#'                             time_col = "ParTime",
#'                             median_mins = median_mins,
#'                             mean_mins = mean_mins)




SmoothTimeSeries <- function(data, id_col = NULL, param_col, var_col, time_col,
                             median_mins = NULL, mean_mins = NULL, n_sec_per_row = 5,
                             k1 = NULL, k2 = NULL) {
  library(zoo)
  library(tidyr)
  library(dplyr)

  if ((!is.null(median_mins) & !is.null(mean_mins)) & (is.null(k1) & is.null(k2))) {
    k1 <- (median_mins * 60)/n_sec_per_row
    k2 <- (mean_mins * 60)/n_sec_per_row
    # values must be odd for the median check. check if that's true and, if not, add 1.
    k1 <- ifelse(k1 %% 2 == 0, yes = k1 + 1, no = k1)
    #k2 <- ifelse(k2 %% 2 == 0, yes = k2 + 1, no = k2)

  } else if ( (is.null(median_mins) & is.null(mean_mins)) & (!is.null(k1) & !is.null(k2))) {
    message("No time specified to calculate median and mean filter over - \n
            assuming data has already been averaged over a time window \n
            so will just use the number of rows specified.")

  } else {
    stop("Need to specify only median and mean mins OR k1 and k2")
  }

  func.list <- list(paste0("rollmedian(x = ", var_col, ", k = ",
                           k1, ", align = 'center', fill = NA)"),
                    paste0("rollmean(x = windowMedian, k = ", k2, ", align = 'center', fill = NA, na.rm = T)"))

  names(func.list) <- c("windowMedian", "windowMean")


  out <- data %>%
    #group_by_(id_col, param_col) %>%
  {if(!is.null(id_col)) group_by_(., id_col, param_col) else group_by_(., param_col)} %>%
    arrange_(time_col) %>%
    mutate_(.dots = func.list[1]) %>%
    fill(windowMedian, .direction = "up") %>%
    fill(windowMedian, .direction = "down") %>%
    mutate_(.dots = func.list[2]) %>%
    fill(windowMean, .direction = "up") %>%
    fill(windowMean, .direction = "down") %>%
    select(-windowMedian) %>%
    rename(ParValueSmth = windowMean)

  return(out)

}
