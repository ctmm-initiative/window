slide <- function(data, CTMM = NULL, window, dt.min = 0, variable = "area", 
                  recycle = FALSE, max_windows = Inf, select = FALSE,
                  Gaussian = FALSE, ...) {
 # data & CTMM arguments can be a list of data.frames or a single data.frame
 if (is.list(data) && all(sapply(data, inherits, "data.frame"))) {
   window_starts <- global_timestamps(data, dt.min, window) # Determine the start of each window
   individual_df <- FALSE
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
 
 # Initialize a list to store extracted Confidence Intervals before converting to dataframe
 results_list <- list()
   
 # Initialize recycled guess object
 previous_model <- vector("list", length(data))  # Creates a list with NULL elements
   
 # Iterate over each window
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
    
    #Extract the data subset
    subset_data <- dataset[start_index[i]:end_index[i], ]
       
     # Recycle previous CTMM model (if recycle = TRUE) or use provided CTMM model
     guess <- if (recycle && !is.null(previous_model[[i]])) previous_model[[i]] else CTMM[[i]]
       
     # Fit models
     FIT <- if (select) ctmm.select(subset_data, guess) else ctmm.fit(subset_data, guess)
       
     # Update previous model after fitting
     if (recycle) {
       previous_model[[i]] <- FIT  
     }
     
     # Compute "akde" (auto-correlated kernel density estimates
     if (variable == "area") {
       window_estimate[[i]] <- akde(subset_data, FIT, ...)
     }
     # Compute speed estimates ('normal' or Gaussian)
     if (variable == "speed") {
       window_estimate[[i]] <- speed(FIT, subset_data, ...)
     }
     if (variable == "speed" && Gaussian) {
       window_estimate[[i]] <- speed(FIT, ...) # stationary Gaussian estimate
     }
     # Save fit_models as window_estimate for simplicity before feeding to meta()
     if (variable == "position") {
       window_estimate[[i]] <- FIT
     }
     if (variable == "velocity") {
       window_estimate[[i]] <- FIT
     }
     if (variable == "diffusion") {
       window_estimate[[i]] <- FIT
     }
   }
   
    # After calculating akde for each dataset in window, filter for valid outputs
   if (variable == "area" && !individual_df) {
     window_estimate <- Filter(function(x) inherits(x, "UD"), window_estimate)
   }
   
    # Feed individual_level estimates into summary()
   if (individual_df) {
     summary_result <- summary(window_estimate[[1]])
     print(summary_result)
     rownames(summary_result$CI) <- c("area", "position", "velocity", "speed", "diffusion")
     CI_low <- summary_result$CI[variable, 1]
     point_estimate <- summary_result$CI[variable, 2]
     CI_high <- summary_result$CI[variable, 3]
   } else {
    # Feed population_level estimates (or fitted model for diffusion, tau position, or velocity) into meta() for each window
     summary_result <- tryCatch(
       meta(window_estimate, variable = variable),
       error = function(e) {
         saveRDS(window_estimate, file = paste0("meta_error_window_", window_start_index, ".rds"))
         print(paste("Error in meta() at window", window_start_index, "Saved error-causing object."))
         return(NULL)  # Return NULL to allow function to continue
       }
     )
     print(summary_result)
     # Save point estimates and confidence intervals (handle null values)
     if (is.null(summary_result)) {
       CI_low <- NA
       point_estimate <- NA
       CI_high <- NA
     }
       else {
         CI_low <- summary_result[1,1]
         point_estimate <- summary_result[1,2]
         CI_high <- summary_result[1,3]
       }
   }
   
 print(point_estimate)
   
# Append to results_list
   results_list[[window_start_index]] <- list(
     timestamp = window_starts[[window_start_index]],
     point_estimate = point_estimate,
     #DOF = DOF_estimate,
     CI_low = CI_low,
     CI_high = CI_high
   )
   if (window_start_index == max_windows)
     break
 }
 # Convert list into data.frame
 results_df <- do.call(rbind, lapply(results_list[1:length(window_starts)], as.data.frame))
 
 # Create the object for the custom class 'TS'
 ts_result <- new("TS", 
                  dataframe = results_df, 
                  variable = variable)
 
 return(ts_result)
}
