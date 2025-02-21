slide <- function(data, CTMM = NULL, window, dt.min = 0, variable = "area", 
                  recycle = FALSE, select = FALSE, ...) {
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
  
  # Initialize start and end indices for each dataset (will move forward every window)
  start_index <- rep(1, length(data))
  end_index <- rep(1, length(data))

  # Initialize a list to store extracted Confidence Intervals before converting to dataframe
  results_list <- list()
  
  # Initialize recycled guess object
  previous_model <- vector("list", length(data))  # Creates a list with NULL elements
  
  # Iterate over each window
  window_count <- 0
  for (window_start_index in seq_along(window_starts)) {
    # Initialize list to store the window estimates from each dataset
    window_estimate <- vector("list", length(data))  
    
    for (i in seq_along(data)) {  # Iterate over each dataset
      dataset <- data[[i]]  # Current dataset
      timestamps <- dataset$timestamp  # Timestamps in the current dataset
      
      # Move start_index forward until within the window
      while (timestamps[start_index[i]] < window_starts[window_start_index] && start_index[i] < length(timestamps)) {
        start_index[i] <- start_index[i] + 1
      }
      
      # Move end_index forward to maintain the window size
      while (timestamps[end_index[i]] <= (window_starts[window_start_index] + window) && end_index[i] < length(timestamps)) {
        end_index[i] <- end_index[i] + 1
      }
      
      # Extract the data subset
      subset_data <- dataset[start_index[i]:end_index[i], ]
      
      # Recycle previous CTMM model (if recycle = TRUE) or use provided CTMM model
      guess <- if (recycle && !is.null(previous_model[[i]])) previous_model[[i]] else CTMM[[i]]
      
      # Fit models
      fitted_model <- if (select) ctmm.select(subset_data, guess) else ctmm.fit(subset_data, guess)
      
      # Update previous model after fitting
      if (recycle) {
        previous_model[[i]] <- fitted_model  
      }
      
      # Compute "akde" (auto-correlated kernel density estimate)
      if (variable == "area") {
        window_estimate[[i]] <- akde(subset_data, fitted_model)
      }
    }
    
     # If individual-level estimate, feed "akde" into summary
    if (individual_df) {
      summary_result <- summary(window_estimate[[1]])
      CI_low <- summary_result$CI[1]
      point_estimate <- summary_result$CI[2]
      CI_high <- summary_result$CI[3]
    } else {
     # If population-level estimate, feed "akde" into meta()
      summary_result <- meta(window_estimate)
      # Save point estimates and confidence intervals
      CI_low <- summary_result[1,1]
      point_estimate <- summary_result[1,2]
      CI_high <- summary_result[1,3]
    }

    # Append to results_list
    results_list[[window_start_index]] <- list(
      timestamp = window_starts[[window_start_index]],
      point_estimate = point_estimate,
      #DOF = DOF_estimate,
      CI_low = CI_low,
      CI_high = CI_high
    )
    window_count <- window_count + 1
  }
  
  # Convert list into data.frame
  results_df <- do.call(rbind, lapply(results_list[1:window_count], as.data.frame))
  
  # Create the object for the custom class 'TS'
  ts_result <- new("TS", 
                   dataframe = results_df, 
                   variable = "area")
  
  return(ts_result)
}
