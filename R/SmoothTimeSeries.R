#' Clean a time series
#'
#' Smooth out time series data using a median and then a mean filter.
#' @param data the input data frame
#' @param id_col name of column, passed as a string, to identify observations by, e.g., a patient identifier
#' @param var_col name of variable to perform the smoothing on passed as a string
#' @param time_col the column containing the timestamps passed as a string
#' @param median_mins window length of data in minutes to perform the median filtering on
#' @param mean_mins window length of data in minutes to perform the mean filtering on
#' @param n_sec_per_row Length of time in seconds that each row of data corresponds to. Defaults to 5 for BedMaster data.
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
#'   SmoothTimeSeries(., id_val = ID, var_col = "Systolic", time_col = "ParTime", median_mins = 30, mean_mins = 30)




SmoothTimeSeries <- function(data, id_col, var_col, time_col, median_mins, mean_mins, n_sec_per_row = 5) {
  library(zoo)
  library(tidyr)
  library(dplyr)

  # calculate number of rows that correspond to the number of minutes in each window
  k1 <- (median_mins * 60) / n_sec_per_row
  k2 <- (mean_mins * 60) / n_sec_per_row

  func.list <- list(
    paste0("rollmedian(x = ", var_col, ", k = ", k1, ", align = 'center', fill = NA)"),
    "windowMedian",
    paste0("rollmean(x = ", var_col, ", k = ", k2, ", align = 'center', fill = NA, na.rm = T)"),
    "windowMean"
  )

  names(func.list) <- c("windowMedian", var_col, "windowMean", var_col)

  out <- data %>%
    group_by_(id_col) %>%
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
