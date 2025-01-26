library(ctmm)

sliding_window <- function(data, CTMM = NULL, window, dt.min = 0, recycle = FALSE) {
  
  # data & CTMM arguments can be a list of data.frames or a single data.frame
  if (is.list(data) && all(sapply(data, inherits, "data.frame"))) {
    window_starts <- global_timestamps(data, dt.min, window) # Determine the start of each window
    individual_df <- FALSE
  } else if (inherits(data, "data.frame")) {
    window_starts <- individual_timestamps(data, dt.min, window)
    CTMM <- list(CTMM)
    data <- list(data)
    individual_df <- TRUE
  } else {
    stop("Error: The object is neither a data frame nor a list of data frames")
  }
  
  # Initialize a vector to store closest timestamps for the current dataset
  aligned_timestamps <- vector("list", length = length(window_starts))
  
  # Iterate over each data set in the input data
  for (i in seq_along(data)) {
    dataset <- data[[i]]  # Current dataset
    timestamps <- dataset$timestamp  # Extract timestamps from dataset
    
    # Iterate over each global start time
    for (j in seq_along(window_starts)) {
      start_time <- window_starts[j]
      
      # Compute the closest timestamp to the global start time
      closest_index <- which.min(abs(timestamps - start_time))
      aligned_timestamps[[i]][j] <- timestamps[closest_index]
    }
  }
  
  # Fit window models and store variable estimates in a dataframe
  population_models <- list()
  results_list <- list() # Save results in list before converting to dataframe
  
  window_count <- length(aligned_timestamps[[1]])  # Use the length of aligned_timestamps[1]
  previous_models <- CTMM  # Initialize with the initial CTMM models
  
  for (window_start_index in 1:window_count) {  # Iterate over window indices
    # Initialize a list to hold individual models for the current window
    current_window_models <- list()
    
    for (i in seq_along(data)) {  # Iterate over each dataset
      dataset <- data[[i]]  # Current dataset
      timestamps <- dataset$timestamp  # Timestamps in the current dataset
      start_times <- aligned_timestamps[[i]]  # Aligned start times for this dataset
      
      # Get the start time for the current window
      start_time <- start_times[window_start_index]
      
      # Calculate bounds
      lower_bound <- window_start_index
      start_time <- as.numeric(start_time, units = "secs") # Convert POSIXct
      window <- as.numeric(window, units = "secs") # Convert Difftime
      upper_bound_value <- start_time + window
      # Return sum to POSIXct
      upper_bound_value <- as.POSIXct(upper_bound_value, origin = "1970-01-01", tz = "UTC")
      upper_bound <- which.min(abs(timestamps - upper_bound_value))
      
      # Subset the data using the upper and lower bounds
      subset_data <- dataset[timestamps >= start_time & timestamps <= upper_bound_value, ]
      
      # Fit a model to the subset
      guess <- if (recycle && !is.null(previous_models)) previous_models[[i]] else CTMM[[i]]
      fitted_model <- ctmm.fit(subset_data, guess)  # Use the previous model if recycle = TRUE
      
      # Save the fitted model
      current_window_models[[i]] <- fitted_model
    }
    
    # Update previous models if recycle = TRUE
    if (recycle) {
      previous_models <- current_window_models
    }
    
    # Run meta() on the current set of models
    if (!individual_df) {
      current_window_model <- meta(current_window_models)
    }
    
    home_range <- akde(subset_data, current_window_models)
    summary_result <- summary(home_range, units = FALSE)
    
    # Save point estimates and confidence intervals
    CI_low <- summary_result$CI[1]
    CI_high <- summary_result$CI[3]
    point_estimate <- summary_result$CI[2]
    DOF_estimate <- summary_result$DOF["area"]
    
    # Append to results_list
    results_list[[window_start_index]] <- list(
      timestamp = window_starts[[window_start_index]],
      point_estimate = point_estimate,
      DOF = DOF_estimate,
      CI_low = CI_low,
      CI_high = CI_high
    )
  }
  # Convert list into data.frame
  results_df <- do.call(rbind, lapply(results_list[1:window_count], as.data.frame))
  
  # Create the object for the custom class 'TS'
  ts_result <- new("TS", 
                   dataframe = results_df, 
                   variable = "area")
  
  return(ts_result)
}
