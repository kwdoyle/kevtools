#' Clean a time series
#'
#' Smooth out time series data using a median and then a mean filter.
#' @param data the input data frame
#' @param var_col name of variable to perform the smoothing on
#' @param time_col the column containing the timestamps
#' @param k1 number of rows to perform the median filtering on
#' @param k2 number of rows to perform the mean filtering on
#' @export
#' @examples
#' ## Doing some pre-cleaning of an arterial blood pressure data set before
#' ## piping to the function
#' tmp <- AllBP_data %>%
#'   filter(ID == id) %>%
#'   group_by(ParTime) %>%
#'   # if multiple measurements at the same time, take the higher one.
#'   filter(Systolic == max(Systolic)) %>%
#'   
#'   SmoothTimeSeries(., "Systolic", "ParTime", k1 = 3, k2 = 12)




SmoothTimeSeries <- function(data, var_col, time_col, k1 = 3, k2 = 12) {
  library(zoo)
  library(tidyr)
  library(dplyr)
  
  func.list <- list(
    paste0("rollmedian(x = ", var_col, ", k = ", k1, ", align = 'center', fill = NA)"),
    "windowMedian",
    paste0("rollmean(x = ", var_col, ", k = ", k2, ", align = 'center', fill = NA, na.rm = T)"),
    "windowMean"
  )
  
  names(func.list) <- c("windowMedian", var_col, "windowMean", var_col)
  
  out <- data %>%
    ungroup() %>%
    arrange_(time_col) %>%
    # Perform median filtering first to replace "spikes" with the median value over a specified number of rows
    mutate_(.dots = func.list[1]) %>%
    # fill the NAs from the rows where there weren't k number of rows before/after to do the rollmedian on
    # with the next closest value
    fill(windowMedian, .direction = "up") %>%
    fill(windowMedian, .direction = "down") %>%
    # overwrite var_col with the new values
    mutate_(.dots = func.list[2]) %>%
    
    ## now do the same for rollmean and replace
    mutate_(.dots = func.list[3]) %>%
    fill(windowMean, .direction = "up") %>%
    fill(windowMean, .direction = "down") %>%
    
    mutate_(.dots=func.list[4])
  
  
  return(out)
  
}