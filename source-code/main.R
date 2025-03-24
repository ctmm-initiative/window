slide <- function(data, CTMM = NULL, window, dt.min = 0, variable = "area", 
                  recycle = FALSE, max_windows = Inf, select = FALSE,
                  Gaussian = FALSE, covariate = NULL, ...) {
  # data & CTMM arguments can be a list of data.frames or a single data.frame
  if (is.list(data) && all(sapply(data, inherits, "data.frame"))) {
    window_starts <- global_timestamps(data, dt.min, window) # Determine the start of each window
    individual_df <- FALSE
    
    # Print range of overlapping time series data for each individual
    message("Sliding window time range: ", as.character(min(window_starts)), " to ", as.character(max(window_starts)))
    
  } else if (inherits(data, "data.frame")) {
    window_starts <- individual_timestamps(data, dt.min, window) # Determine the start of each window
    CTMM <- list(CTMM)
    data <- list(data)
    individual_df <- TRUE
  } else {
    stop("Error: The object is neither a data frame nor a list of data frames")
  }
  # Initialize start and end indices for each dataset (will move forward every window)
  start_index <- rep(1, length(data))
  end_index <- rep(1, length(data))
  
  results_list <- list() # Initialize a list to store extracted Confidence Intervals before converting to dataframe
  
  previous_model <- vector("list", length(data)) # Initialize recycled guess object
  
  # Iterate over each window
  for (window_start_index in seq_along(window_starts)) {
    window_estimate <- vector("list", length(data)) # Initialize list to store the window estimates from each dataset
    mean_covariate <- numeric(length(data)) # Initialize numeric vector to store the covariate estimates from each dataset
    # Stop if window index exceeds the user-defined max
    if (window_start_index > max_windows) {
      message("Reached max_windows limit: ", max_windows)
      break
    }
    # Iterate over each dataset
    for (i in seq_along(data)) {
      dataset <- data[[i]] # Current dataset
      timestamps <- dataset$timestamp # Timestamps in the current dataset
      
      # Move start_index forward past window_start
      while (timestamps[start_index[i]] < window_starts[window_start_index] && start_index[i] < length(timestamps)) {
        start_index[i] <- start_index[i] + 1
      }
      # Move end_index forward untill past (window_start + window)
      while (timestamps[end_index[i]] <= (window_starts[window_start_index] + window) && end_index[i] < length(timestamps)) {
        end_index[i] <- end_index[i] + 1
      }
      
      # Subset data
      subset_data <- dataset[start_index[i]:end_index[i], ]
      start_time <- timestamps[start_index[i]]
      end_time <- timestamps[end_index[i]]
      window_duration <- as.numeric(window, units = "secs")
      coverage <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      # Detect and skip large gaps in timestamp coverage
      if (coverage < 0.5 * window_duration) {
        message(paste("Skipping gap in dataset", i, "at window", window_start_index))
        window_estimate[[i]] <- NULL
        next
      }
      
      # Recycle previous CTMM model (if recycle = TRUE) or use provided CTMM model
      guess <- if (recycle && !is.null(previous_model[[i]])) previous_model[[i]] else CTMM[[i]]
      # Fit models
      FIT <- if (select) ctmm.select(subset_data, guess) else ctmm.fit(subset_data, guess)
      # Update previous model after fitting
      if (recycle) previous_model[[i]] <- FIT
      
      # Compute estimates based on the variable
      if (variable == "area") {
        window_estimate[[i]] <- akde(subset_data, FIT, ...)
      }
      if (variable == "speed") {
        window_estimate[[i]] <- speed(FIT, subset_data, ...)
        if (Gaussian) window_estimate[[i]] <- speed(FIT, ...) # stationary Gaussian estimate
      }
      if (variable %in% c("position", "velocity", "diffusion")) {
        window_estimate[[i]] <- FIT
      }
      
      # If a covariate is provided, extract the mean for the subset
      if (!is.null(covariate)) {
        covariate_fixed <- gsub("-", ".", covariate) # Convert dashes to dots for compatibility
        if (covariate_fixed %in% colnames(subset_data)) {
          mean_covariate[i] <- mean(subset_data[[covariate_fixed]], na.rm = TRUE)
        } else {
          message(paste("Warning: Covariate", covariate_fixed, "not found in dataset", i))
          mean_covariate[i] <- NA_real_
        }
      }
    }
    # Calculate the covariate mean for the current window
    window_covariate <- if (!is.null(covariate)) {
      if (all(is.na(mean_covariate))) NA_real_ else mean(mean_covariate, na.rm = TRUE)
    } else {
      NA_real_ # Set to NA if no covariate is provided
    }
    
    # Process results
    if (variable == "area" && !individual_df) {
      window_estimate <- Filter(function(x) inherits(x, "UD"), window_estimate)
    }
    # After calculating akde for each dataset in window, filter for valid outputs
    window_estimate <- Filter(Negate(is.null), window_estimate)
    
    if (length(window_estimate) == 0) {
      point_estimate <- NA
      CI_low <- NA
      CI_high <- NA
    } else if (individual_df) {
      summary_result <- summary(window_estimate[[1]])
      # Rename columns for compatability
      rownames(summary_result$CI) <- c("area", "position", "velocity", "speed", "diffusion")
      CI_low <- summary_result$CI[variable, 1]
      point_estimate <- summary_result$CI[variable, 2]
      CI_high <- summary_result$CI[variable, 3]
    } else {
      # Feed population-level estimates (or fitted model for diffusion, tau position, or velocity) into meta() for each window
      summary_result <- tryCatch(
        meta(window_estimate, variable = variable),
        error = function(e) {
          saveRDS(window_estimate, file = paste0("meta_error_window_", window_start_index, ".rds"))
          message(paste("Error in meta() at window", window_start_index, "- Saved error-causing object."))
          return(NULL)
        }
      )
      # Save point estimates and confidence intervals (handle null values)
      if (is.null(summary_result)) {
        CI_low <- NA
        point_estimate <- NA
        CI_high <- NA
      } else {
        CI_low <- summary_result[1, 1]
        point_estimate <- summary_result[1, 2]
        CI_high <- summary_result[1, 3]
      }
    }
    # Append to results_list
    results_list[[window_start_index]] <- list(
      timestamp = window_starts[[window_start_index]],
      point_estimate = point_estimate,
      CI_low = CI_low,
      CI_high = CI_high,
      covariate = window_covariate
    )
  }
  # Convert list into data.frame
  results_df <- do.call(rbind, lapply(results_list, as.data.frame))
  
  # Create the object for the custom class 'TS'
  ts_result <- new("TS", 
                   dataframe = results_df, 
                   variable = variable,
                   covariate = as.numeric(results_df$covariate))
  
  return(ts_result)
}
