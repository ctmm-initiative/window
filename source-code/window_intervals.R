#window intervals if data argument is a list of datasets
global_timestamps <- function(data_list, dt.min, window) {
  
  # Extract timestamps from each dataset
  timestamps <- lapply(data_list, function(data) as.POSIXct(data$timestamp))
  
  # Calculate global start and end times
  global_start <- max(sapply(timestamps, min))  # Max of the minimum timestamps
  global_end <- min(sapply(timestamps, max))    # Min of the maximum timestamps
  
  # Ensure global_start is before global_end
  if (global_start > global_end) {
    stop("Datasets do not overlap")
  }
  
  # Ensure parameters are in seconds before running calculations
  step_size <- as.numeric(dt.min, units = "secs")
  window <- as.numeric(window, units = "secs")
  
  # Generate sequence of aligned timestamps
  length.out <- (global_end - global_start) / step_size + 1
  length.out <- ceiling(length.out)  # Rounds up to ensure no truncation
  aligned_window_start <- seq(from = global_start, to = global_end, length.out = length.out)
  
  # Trim aligned_window_start so that no window exceeds global_end
  aligned_window_start <- aligned_window_start[aligned_window_start + window <= global_end]
  
  # Returns aligned_window_start to POXIXct format
  aligned_window_start <- as.POSIXct(aligned_window_start, origin = "1970-01-01", tz = "UTC")
  
  
  return(aligned_window_start)
}

#Window_intervals if data argument is individual dataset
individual_timestamps <- function(data, dt.min, window) {
  # Extract timestamps from individual dataset
  timestamps <- as.POSIXct(data$timestamp)
  
  # Calculate start and end times
  start <- min(timestamps)
  end <- max(timestamps)
  
  
  # Ensure parameters are in seconds before running calculations
  step_size <- as.numeric(dt.min, units = "secs")
  window <- as.numeric(window, units = "secs")
  
  # Generate sequence of aligned timestamps
  window_start <- seq(from = start, to = end, by = step_size)
  
  # Trim window_start so that no window exceeds global_end
  window_start <- window_start[window_start + window <= end]
  
  # Returns window_start to POXIXct format
  window_start <- as.POSIXct(window_start, origin = "1970-01-01", tz = "UTC")
  
  
  return(window_start)
}