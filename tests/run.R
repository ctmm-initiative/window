# run.R

# Load necessary functions
source("TS_class.R")
source("sliding_window_function.R")

run_analysis <- function(data, window, min_time_step, variable = "area", max_windows = 1000, select = FALSE, recycle = FALSE) {
  
  # Guess the initial CTMM model
  GUESS <- ctmm.guess(data, interactive = FALSE)
  CTMM <- ctmm.fit(data, GUESS)
  
  # Call the sliding window function
  results <- sliding_window(data = data, CTMM = CTMM, window = window, 
                            dt.min = min_time_step, max_windows = max_windows, 
                            select = select, recycle = recycle, variable = variable)
  
  # Plot the results
  plot(results)
  
  return(results)
}

# Example usage: Call the function with your data
data(buffalo)
Data <- buffalo$Cilla
min_time_step <- as.difftime(10, units = "days")
window <- as.difftime(10, units = "days")

# Run the analysis
results <- run_analysis(data = Data, window = window, min_time_step = min_time_step)
