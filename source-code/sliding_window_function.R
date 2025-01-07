# sliding_window_function.R

sliding_window <- function(data, CTMM = NULL, window, dt.min = 0, max_windows = 1000, 
                           select = TRUE, recycle = FALSE, variable = "area", units = TRUE) {
  window <- as.numeric(window, units = "secs")
  dt.min <- as.numeric(dt.min, units = "secs")
  if (is.null(CTMM)) CTMM <- ctmm.guess(data, interactive = FALSE)
  
  # Initialize list to store results
  results_list <- list()
  
  DATA <- data
  timestamps <- as.POSIXct(DATA[[1]])  # Ensure timestamps are in POSIXct format
  
  # Determine the end of the loop to prevent windows from exceeding data boundary
  end <- NA
  
  for (i in seq_along(timestamps)) {
    time_diff1 <- as.numeric(difftime(timestamps[length(timestamps)], timestamps[i], units = "secs"))
    if (time_diff1 <= window) {
      end <- i - 1
      print("found end")
      print(end)
      break
    }
  }
  
  # Sliding window logic
  window_count <- 0
  
  # Determine window_start
  for (window_start in seq(1, end)) {
    if (window_start == 1) {
    } else {
      timediff2 <- as.numeric(difftime(timestamps[window_start], timestamps[prev_window_start], units = "secs"))
      if (timediff2 <= as.numeric(dt.min, units = "secs")) {
        next
      }
    }
    
    prev_window_start <- window_start
    print("window start:")
    print(window_start)
    
    # Determine window_end
    window_end <- window_start + 1
    while (as.numeric(difftime(timestamps[window_end], timestamps[window_start], units = "secs")) <= window) {
      window_end <- window_end + 1
    }
    print("window end:")
    print(window_end)
    
    # Increment window counter
    window_count <- window_count + 1
    #if (window_count > max_windows) break
    
    # Fit windows
    DATA_sub <- DATA[window_start:window_end, ]
    CTMM_result <-  if (select) ctmm.select(DATA_sub, CTMM) else ctmm.fit(DATA_sub, CTMM)
    akde_result <- akde(DATA_sub, CTMM_result)
    summary_result <- summary(akde_result, units = FALSE)
    
    # If recycle is true, use the previous model as starting point for fitting next model.
    CTMM <- if (recycle) CTMM_result
    
    # Save point estimates and confidence intervals
    CI_low <- summary_result$CI[1]
    CI_high <- summary_result$CI[3]
    point_estimate <- summary_result$CI[2]
    DOF_estimate <- summary_result$DOF["area"]
    
    middle_timestamp <- mean(timestamps[window_start:window_end])
    epoch <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")  # The EPOCH time (global reference)
    middle_timestamp <- as.POSIXct(middle_timestamp, origin = epoch)
    
    # Append to results_list
    results_list[[window_count]] <- list(
      timestamp = middle_timestamp,
      point_estimate = point_estimate,
      DOF = DOF_estimate,
      CI_low = CI_low,
      CI_high = CI_high
    )
    print(results_list)
  }
  
  results_df <- do.call(rbind, lapply(results_list[1:window_count], as.data.frame))
  row.names(results_df) <- NULL
  print(results_df)
  
  # Create the object for the custom class 'TS'
  ts_results <- new("TS", 
                    dataframe = results_df, 
                    variable = variable)
  
  return(ts_results)
}
