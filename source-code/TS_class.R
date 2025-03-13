# Define S4 class with dimensional map and optional covariate slot
setClass(
  "TS",
  slots = list(
    dataframe = "data.frame",  # Main data frame for tracking data
    variable = "character",    # Variable of interest (e.g., "speed")
    dimension_map = "list",    # Mapping for dimension conversion (time, lat, lon, etc.)
    covariate = "numeric"     # New slot for storing covariate (e.g., temperature)
  ),
  prototype = list(
    dimension_map = list(
      "speed" = "time",          # Map speed to time dimension
      "position" = c("lat", "long"),  # Map position to lat/long dimensions
      "diffusion" = "diffusion", # Map diffusion to diffusion
      "velocity" = "velocity"   # Map velocity to time
    ),
    covariate = numeric(0)      # Default empty covariate (temperature, etc.)
  )
)

# Define the plot method for the TS class with covariate
setMethod(
  "plot", 
  "TS",
  function(x, col = "black", cov_col = "red", ylab = "", ...) {
    # Extract the data and variable from the TS object
    data <- x@dataframe
    variable <- x@variable
    covariate <- x@covariate  # Extract the covariate (e.g., temperature)
    
    # Handle X-axis (time)
    t <- data$timestamp
    
    # Handle Y-axis for movement variable
    CI <- c(data$point_estimate, data$CI_low, data$CI_high)  # Combine columns for dimfig
    # Extract dimension type from the map
    dimension_type <- x@dimension_map[[variable]]
    # Use dimfig() with the mapped dimension
    dimfig_output <- dimfig(data = CI, dimension = dimension_type)
    units_label <- dimfig_output$units[2]  # Use the short unit label for units
    ylab <- paste0(toupper(substr(variable, 1, 1)), substr(variable, 2, nchar(variable)), " (", units_label, ")")
    
    # Update dataframe values to converted units
    data$point_estimate <- dimfig_output$data[1:nrow(data)]
    data$CI_low <- dimfig_output$data[(nrow(data) + 1):(2 * nrow(data))]
    data$CI_high <- dimfig_output$data[(2 * nrow(data) + 1):(3 * nrow(data))]
    
    # Extract the columns for plotting
    CI <- data[, c("point_estimate", "CI_low", "CI_high")]
    MAX <- max(CI$point_estimate, na.rm = TRUE)
    MIN <- min(CI$point_estimate, na.rm = TRUE)
    DIFF <- MAX - MIN
    ylim <- c(max(MIN - DIFF, min(CI$CI_low, na.rm = TRUE)),
              min(MAX + DIFF, max(CI$CI_high, na.rm = TRUE)))
    
    # Plot the central estimate and Confidence Intervals for the movement variable
    graphics::plot(t, data$point_estimate, type = "l", col = col, ylim = ylim,
                   xlab = "Time", ylab = ylab, ...)
    graphics::lines(t, data$CI_low, col = adjustcolor(col, alpha.f = 0.3), lty = 2, ...)
    graphics::lines(t, data$CI_high, col = adjustcolor(col, alpha.f = 0.3), lty = 2, ...)
    
    # Plot the covariate (e.g., temperature) as a second Y-axis
    if (length(covariate) > 0) {
      # Normalize covariate for plotting on the same scale
      cov_max <- max(covariate, na.rm = TRUE)
      cov_min <- min(covariate, na.rm = TRUE)
      cov_diff <- cov_max - cov_min
      cov_ylimits <- c(cov_min - 0.1 * cov_diff, cov_max + 0.1 * cov_diff)
      
      # Plot covariate on a new axis
      par(new = TRUE)  # Allow for overlay of the second plot
      plot(t, covariate, type = "l", col = cov_col, axes = FALSE, ylim = cov_ylimits, 
           xlab = "", ylab = "", ...)
      
      # Add a secondary Y-axis on the right side for the covariate
      axis(4, at = seq(cov_min, cov_max, length.out = 5), col = cov_col, col.axis = cov_col)
      mtext("Temperature (°C)", side = 4, line = 3, col = cov_col)
    }
  }
)